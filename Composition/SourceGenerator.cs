using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace TA.Util.Composition
{
	[Generator]
	public class SourceGenerator : ISourceGenerator
	{
		public void Initialize(GeneratorInitializationContext context)
		{
		}
		public void Execute(GeneratorExecutionContext context)
		{
			foreach (var syntaxTree in context.Compilation.SyntaxTrees)
			{
				SyntaxTree(context, syntaxTree);
			}
		}

		record struct TypePrefixes(List<string> Prefixes, string SourcePrefix, string SourceSuffix, string FilePrefix, string NamePrefix);
		record struct CompositionBaseNextBase(TypeSyntax BaseTypeSyntax, INamedTypeSymbol BaseTypeSymbol, string FieldPrefix, object Parent);
		record struct CompositionBase(TypeSyntax BaseTypeSyntax, INamedTypeSymbol BaseTypeSymbol, List<CompositionBaseNextBase> NextBases);
		record struct BaseInterfaceMembers(TypeSyntax TypeSyntax, INamedTypeSymbol TypeSymbol, IReadOnlyList<ISymbol> Members);
		delegate void WriteTypeParameterDelegate(StringBuilder sb, ITypeParameterSymbol typeParameterSymbol);

		static void SyntaxTree(GeneratorExecutionContext context, SyntaxTree syntaxTree)
		{
			foreach (var typeDeclaration in syntaxTree.GetRoot().DescendantNodesAndSelf().OfType<TypeDeclarationSyntax>())
			{
				TypeDeclaration(new TypeDeclarationContext(context, syntaxTree, typeDeclaration));
			}
		}

		record struct TypeDeclarationContext(GeneratorExecutionContext context, SyntaxTree syntaxTree, TypeDeclarationSyntax typeDeclaration);
		static void TypeDeclaration(TypeDeclarationContext context)
		{
			try
			{
				TypePrefixes? typePrefixes = null;
				TypePrefixes GetTypePrefixes()
				{
					if (typePrefixes == null) typePrefixes = TypeDeclaration_TypePrefixes(context);
					return typePrefixes.Value;
				}

				var compositionBases = new List<CompositionBase>();
				bool isPart = false;
				foreach (var attribute in context.typeDeclaration.AttributeLists.SelectMany(als => als.Attributes))
				{
					if (attribute.Name.GetText().ToString() == "CompositionPart" || attribute.Name.GetText().ToString() == "CompositionPartAttribute")
					{
						isPart = true;
						continue;
					}

					var baseTypeSyntaxAndSymbol = TypeDeclaration_ComposedOfAttribute(context.context, GetTypePrefixes, attribute);
					if (baseTypeSyntaxAndSymbol == null)
						continue;

					var compositionBase = new CompositionBase(baseTypeSyntaxAndSymbol.Value.baseTypeSyntax, baseTypeSyntaxAndSymbol.Value.baseTypeSymbol, new List<CompositionBaseNextBase>());
					TypeDeclaration_NextBaseTypeSymbols(context.context, compositionBase, baseTypeSyntaxAndSymbol.Value.baseTypeSymbol);

					compositionBases.Add(compositionBase);
				}
				if (compositionBases.Count > 0 || isPart)
				{
					TypeDeclaration_Create(context.context, context.typeDeclaration, GetTypePrefixes(), compositionBases, isPart);
				}
			}
			catch (Exception ex)
			{
				context.context.ReportDiagnostic(Diagnostic.Create(
					new DiagnosticDescriptor("TAC0001", "Failed to generate composition", "{0}", "TA.Composition", DiagnosticSeverity.Error, true),
					null,
					ex.Message
				));
			}
		}
		static (TypeSyntax baseTypeSyntax, INamedTypeSymbol baseTypeSymbol)? TypeDeclaration_ComposedOfAttribute(GeneratorExecutionContext context, Func<TypePrefixes> getTypePrefixes, AttributeSyntax attribute)
		{
			if (
				(attribute.Name.GetText().ToString() != "ComposedOf" && attribute.Name.GetText().ToString() != "ComposedOfAttribute") ||
				attribute.ArgumentList == null ||
				attribute.ArgumentList.Arguments.Count < 1 ||
				attribute.ArgumentList.Arguments.Count > 2
			)
				return null;

			TypeSyntax baseTypeSyntax;

			if (attribute.ArgumentList.Arguments.Count == 1 && attribute.ArgumentList.Arguments[0].Expression is TypeOfExpressionSyntax typeOfExpression)
			{
				baseTypeSyntax = typeOfExpression.Type;
			}
			else if (
				attribute.ArgumentList.Arguments.Count == 2 &&
				attribute.ArgumentList.Arguments[0].Expression is TypeOfExpressionSyntax typeOfExpressionGeneric &&
				attribute.ArgumentList.Arguments[1].Expression is LiteralExpressionSyntax literalExpressionSyntax &&
				literalExpressionSyntax.IsKind(SyntaxKind.StringLiteralExpression))
			{
				string typeOfExpressionStr = typeOfExpressionGeneric.Type.ToFullString().Trim();
				baseTypeSyntax =
					SyntaxFactory.ParseTypeName(
						typeOfExpressionStr.Substring(0, typeOfExpressionStr.IndexOf('<')) +
						literalExpressionSyntax.ToFullString().Trim('"')
					);
			}
			else
				return null;

			var baseTypeSymbol = TypeDeclaration_GetTypeSymbol(context, getTypePrefixes(), baseTypeSyntax);
			if (baseTypeSymbol == null) return null;

			return (baseTypeSyntax, baseTypeSymbol);
		}
		static void TypeDeclaration_NextBaseTypeSymbols(GeneratorExecutionContext context, CompositionBase compositionBase, INamedTypeSymbol baseTypeSymbol)
		{
			void FillBasesRecursive(ITypeSymbol typeSymbol, string fieldPrefix, object parent)
			{
				var attributes = typeSymbol.GetAttributes();
				foreach (var attribute in attributes)
				{
					if (attribute.AttributeClass?.Name != "ComposedOf" && attribute.AttributeClass?.Name != "ComposedOfAttribute")
						continue;
					TypeSyntax nextBaseTypeSyntax;
					var nextBaseTypeSymbol = (INamedTypeSymbol)attribute.ConstructorArguments[0].Value!;
					if (attribute.ConstructorArguments.Length > 1 && attribute.ConstructorArguments[1].Value is string genericsString)
					{
						if (attribute.ApplicationSyntaxReference != null)
						{
							var attributeSyntax = (AttributeSyntax)attribute.ApplicationSyntaxReference.GetSyntax();
							var typeOfExpressionGeneric = (TypeOfExpressionSyntax)attributeSyntax.ArgumentList!.Arguments[0].Expression;
							var literalExpressionSyntax = (LiteralExpressionSyntax)attributeSyntax.ArgumentList.Arguments[1].Expression;
							string typeOfExpressionStr = typeOfExpressionGeneric.Type.ToFullString().Trim();
							nextBaseTypeSyntax =
								SyntaxFactory.ParseTypeName(
									typeOfExpressionStr.Substring(0, typeOfExpressionStr.IndexOf('<')) +
									literalExpressionSyntax.ToFullString().Trim('"')
								);
							nextBaseTypeSymbol =
								TypeDeclaration_GetTypeSymbol(
									context,
									TypeDeclaration_TypePrefixes(new TypeDeclarationContext(context, attribute.ApplicationSyntaxReference.SyntaxTree, (TypeDeclarationSyntax)attributeSyntax.Parent!.Parent!)),
									nextBaseTypeSyntax
								);
							if (nextBaseTypeSymbol == null) continue;
						}
						else
						{
							throw new NotImplementedException();
						}
					}
					else
					{
						nextBaseTypeSyntax = SyntaxFactory.ParseTypeName(nextBaseTypeSymbol.ToDisplayString().Trim());
					}
					if (!compositionBase.NextBases.Any(x => SymbolEqualityComparer.Default.Equals(x.BaseTypeSymbol, nextBaseTypeSymbol)))
					{
						var nextBase = new CompositionBaseNextBase(nextBaseTypeSyntax, nextBaseTypeSymbol, fieldPrefix, parent);
						compositionBase.NextBases.Add(nextBase);
						FillBasesRecursive(nextBaseTypeSymbol, fieldPrefix + nextBaseTypeSymbol.Name + ".", nextBase);
					}
				}
			}
			FillBasesRecursive(baseTypeSymbol, baseTypeSymbol.Name + ".", compositionBase);
		}
		static INamedTypeSymbol? TypeDeclaration_GetTypeSymbol(GeneratorExecutionContext context, TypePrefixes typePrefixes, TypeSyntax typeSyntax)
		{
			string typeName = typeSyntax.ToFullString().Trim();
			string typeNameTry = typeName;

			int angleBracketPos = typeNameTry.IndexOf('<');
			if (angleBracketPos != -1)
			{
				int arity = 1, nesting = 0;
				for (int i = angleBracketPos + 1; i < typeNameTry.Length; i++)
				{
					switch (typeNameTry[i])
					{
						case '<':
							++nesting;
							break;
						case '>':
							if (nesting > 0)
								--nesting;
							break;
						case ',':
							if (nesting == 0) ++arity; break;
					}
				}
				typeNameTry = typeNameTry.Substring(0, angleBracketPos) + "`" + arity;
			}

			while (true)
			{
				foreach (string prefix in typePrefixes.Prefixes)
				{
					var typeSymbol = context.Compilation.GetTypeByMetadataName(prefix + typeNameTry);
					if (typeSymbol != null)
						return typeSymbol;
				}
				int dotPos = typeNameTry.LastIndexOf('.');
				if (dotPos == -1) break;
				typeNameTry = typeNameTry.Substring(0, dotPos) + "+" + typeNameTry.Substring(dotPos + 1);
			}
			return null;
		}
		static TypePrefixes TypeDeclaration_TypePrefixes(TypeDeclarationContext context)
		{
			var prefixes = new List<string> { "" };
			var current = context.typeDeclaration.Parent;
			string sourcePrefix = "", sourceSuffix = "", filePrefix = "", namePrefix = "";
			while (current != null)
			{
				if (current is TypeDeclarationSyntax currentTypeDeclaration)
				{
					sourcePrefix = $@"partial {SyntaxFacts.GetText(currentTypeDeclaration.Keyword.Kind())} {currentTypeDeclaration.Identifier.Text}
{{
{sourcePrefix}";
					sourceSuffix = $@"{sourceSuffix}
}}
";
					filePrefix = currentTypeDeclaration.Identifier.Text + "." + filePrefix;
					namePrefix = currentTypeDeclaration.Identifier.Text + (currentTypeDeclaration.Arity > 0 ? "`" + currentTypeDeclaration.Arity : "") + "+" + namePrefix;

					prefixes.Add(currentTypeDeclaration.Identifier.Text + "+" + prefixes.Last());
				}
				else if (current is NamespaceDeclarationSyntax currentNamespaceDeclaration)
				{
					sourcePrefix = $@"namespace {currentNamespaceDeclaration.Name}
{{
{sourcePrefix}";
					sourceSuffix = $@"{sourceSuffix}
}}
";
					filePrefix = currentNamespaceDeclaration.Name + "." + filePrefix;
					namePrefix = currentNamespaceDeclaration.Name + "." + namePrefix;
					prefixes.AddRange(prefixes.Select(s => currentNamespaceDeclaration.Name.ToFullString().Trim() + "." + s).ToList());
				}
				current = current.Parent;
			}
			foreach (var usingSyntax in context.syntaxTree.GetRoot().DescendantNodesAndSelf().OfType<UsingDirectiveSyntax>())
			{
				prefixes.Add(usingSyntax.Name.ToFullString() + ".");
			}
			return new TypePrefixes(prefixes, sourcePrefix, sourceSuffix, filePrefix, namePrefix);
		}
		static void TypeDeclaration_Create(GeneratorExecutionContext context, TypeDeclarationSyntax typeDeclaration, TypePrefixes typePrefixes, List<CompositionBase> compositionBases, bool isPart)
		{
			string typeNameAndGenerics = typeDeclaration.Identifier.Text + typeDeclaration.TypeParameterList?.ToFullString().Trim();
			var sb = new StringBuilder();

			sb.AppendLine("#nullable enable");
			TypeDeclaration_Create_Usings(typeDeclaration, compositionBases, isPart, sb);

			sb.AppendLine();
			sb.Append(typePrefixes.SourcePrefix);

			if (compositionBases.Count > 0)
			{
				TypeDeclaration_Create_TypeStart(typeDeclaration, compositionBases, typeNameAndGenerics, sb);

				var emittedSymbols = new List<ISymbol>();
				foreach (var compositionBase in compositionBases)
				{
					TypeDeclaration_Create_Base(context, typeDeclaration, typePrefixes, typeNameAndGenerics, sb, emittedSymbols, compositionBase);
				}

				TypeDeclaration_Create_EqualsObject(context, typeDeclaration, typePrefixes, compositionBases, sb);

				sb.AppendLine(" }");
			}

			if (isPart)
			{
				TypeDeclaration_Create_Part(context, typeDeclaration, typePrefixes, compositionBases, typeNameAndGenerics, sb);
			}

			sb.Append(typePrefixes.SourceSuffix);

			context.AddSource($"{typePrefixes.FilePrefix}{typeNameAndGenerics.Replace("<", "_").Replace(">", "_")}.g.cs", sb.ToString());
		}
		static void TypeDeclaration_Create_Usings(TypeDeclarationSyntax typeDeclaration, List<CompositionBase> compositionBases, bool isPart, StringBuilder sb)
		{
			sb.AppendLine("using System;");
			sb.AppendLine("using System.Diagnostics.CodeAnalysis;");
			if (isPart)
			{
				foreach (var usingSyntax in typeDeclaration.SyntaxTree.GetRoot().DescendantNodesAndSelf().OfType<UsingDirectiveSyntax>())
				{
					if (usingSyntax.Name.ToFullString() == "System" || usingSyntax.Name.ToFullString() == "System.Diagnostics.CodeAnalysis")
						continue;
					sb.Append("using ");
					sb.Append(usingSyntax.Name.ToFullString());
					sb.AppendLine(";");
				}
			}
			foreach (var compositionBase in compositionBases)
			{
				sb.Append("// using for: ");
				sb.AppendLine(compositionBase.BaseTypeSymbol.Name);
				sb.Append("using ");
				sb.Append(compositionBase.BaseTypeSymbol.ContainingNamespace.ToDisplayString());
				sb.AppendLine(";");
			}
		}
		static void TypeDeclaration_Create_TypeStart(TypeDeclarationSyntax typeDeclaration, List<CompositionBase> compositionBases, string typeNameAndGenerics, StringBuilder sb)
		{
			sb.AppendLine("#pragma warning disable CS0282 // There is no defined ordering between fields in multiple declarations of partial struct");

			sb.Append("	partial ");
			sb.Append(SyntaxFacts.GetText(typeDeclaration.Keyword.Kind()));
			sb.Append(' ');
			sb.Append(typeNameAndGenerics);
			sb.Append(" : ");
			bool first = true;
			foreach (var compositionBase in compositionBases)
			{
				string baseTypeReference = compositionBase.BaseTypeSyntax.ToFullString();

				if (first) first = false; else sb.Append(", ");
				sb.Append("IEquatable<");
				sb.Append(baseTypeReference);
				sb.Append(">, ");
				if (
					compositionBase.BaseTypeSymbol.GetAttributes()
						.Any(ad => ad.AttributeClass?.Name == "CompositionPart" || ad.AttributeClass?.Name == "CompositionPartAttribute")
				)
				{
					WriteCompositionInterfaceReference(sb, baseTypeReference);
				}
				else
				{
					sb.Append("IComposedOf<");
					sb.Append(baseTypeReference);
					sb.Append('>');
					foreach (var nextBase in compositionBase.NextBases)
					{
						sb.Append(", IComposedOf<");
						TypeDeclaration_Create_Base_WriteType(sb, (sb, tps) => TypeDeclaration_Create_Base_WriteType_GenericParameter(sb, tps, nextBase), nextBase.BaseTypeSymbol);
						sb.Append('>');
					}
				}
			}
			sb.AppendLine();

			sb.AppendLine("#pragma warning restore CS0282 // There is no defined ordering between fields in multiple declarations of partial struct");
			sb.AppendLine("	{");
		}
		static void WriteCompositionInterfaceReference(StringBuilder sb, string typeReference)
		{
			string baseTypeReferenceForInterface = typeReference;
			// check for namespace
			int dotPos = typeReference.LastIndexOf('.');
			// only if dot is before first generics <
			int angleBracketPos = typeReference.IndexOf('<');
			if (dotPos != -1 && (angleBracketPos == -1 || dotPos < angleBracketPos))
			{
				sb.Append(typeReference.Substring(0, dotPos + 1));
				baseTypeReferenceForInterface = typeReference.Substring(dotPos + 1);
			}
			WriteCompositionInterfaceNameAndGenerics(sb, baseTypeReferenceForInterface);
		}
		static void WriteCompositionInterfaceNameAndGenerics(StringBuilder sb, string typeNameAndGenerics)
		{
			sb.Append('I');
			int angleBracketPos = typeNameAndGenerics.IndexOf('<');
			if (angleBracketPos != -1)
			{
				sb.Append(typeNameAndGenerics.Substring(0, angleBracketPos));
				sb.Append("Composition");
				sb.Append(typeNameAndGenerics.Substring(angleBracketPos));
			}
			else
			{
				sb.Append(typeNameAndGenerics);
				sb.Append("Composition");
			}
		}
		#region base
		static void TypeDeclaration_Create_Base(GeneratorExecutionContext context, TypeDeclarationSyntax typeDeclaration, TypePrefixes typePrefixes, string typeNameAndGenerics, StringBuilder sb, List<ISymbol> emittedSymbols, CompositionBase compositionBase)
		{
			string baseTypeName = compositionBase.BaseTypeSymbol.Name;
			string baseTypeReference = compositionBase.BaseTypeSyntax.ToFullString();

			sb.Append("\t\t// Base type: ");
			sb.AppendLine(baseTypeReference);

			TypeDeclaration_Create_Base_Field(sb, baseTypeName, baseTypeReference);
			TypeDeclaration_Create_Base_Operator(typeNameAndGenerics, sb, baseTypeName, baseTypeReference);
			TypeDeclaration_Create_Base_IComposedOf(sb, (sb, tps) => TypeDeclaration_Create_Base_WriteType_GenericParameter(sb, tps, compositionBase), compositionBase.BaseTypeSymbol, baseTypeName);
			foreach (var nextBase in compositionBase.NextBases)
			{
				TypeDeclaration_Create_Base_IComposedOf(sb, (sb, tps) => TypeDeclaration_Create_Base_WriteType_GenericParameter(sb, tps, nextBase), nextBase.BaseTypeSymbol, nextBase.FieldPrefix + nextBase.BaseTypeSymbol.Name);
			}
			TypeDeclaration_Create_Base_IEquatable(sb, compositionBase, baseTypeName, baseTypeReference);

			sb.AppendLine("\t\t// Constructors");
			foreach (var constructor in compositionBase.BaseTypeSymbol.Constructors)
			{
				TypeDeclaration_Create_Base_Constructor(sb, compositionBase, baseTypeName, baseTypeReference, constructor);
			}

			TypeDeclaration_Create_Base_MembersRecursive(context, typeDeclaration, typePrefixes, sb, emittedSymbols, (sb, tps) => TypeDeclaration_Create_Base_WriteType_GenericParameter(sb, tps, compositionBase), compositionBase.BaseTypeSymbol, baseTypeName, baseTypeReference);
			foreach (var nextBase in compositionBase.NextBases)
			{
				var sbBaseTypeReference = new StringBuilder();
				TypeDeclaration_Create_Base_WriteType(sbBaseTypeReference, (sb, tps) => TypeDeclaration_Create_Base_WriteType_GenericParameter(sb, tps, nextBase), nextBase.BaseTypeSymbol);
				TypeDeclaration_Create_Base_MembersRecursive(context, typeDeclaration, typePrefixes, sb, emittedSymbols, (sb, tps) => TypeDeclaration_Create_Base_WriteType_GenericParameter(sb, tps, nextBase), nextBase.BaseTypeSymbol, nextBase.FieldPrefix + nextBase.BaseTypeSymbol.Name, sbBaseTypeReference.ToString());
			}

			sb.AppendLine();
		}
		static void TypeDeclaration_Create_Base_Field(StringBuilder sb, string baseTypeName, string baseTypeReference)
		{
			sb.Append("\t\tpublic ");
			sb.Append(baseTypeReference);
			sb.Append(' ');
			sb.Append(baseTypeName);
			sb.AppendLine(";");
		}
		static void TypeDeclaration_Create_Base_Operator(string typeNameAndGenerics, StringBuilder sb, string baseTypeName, string baseTypeReference)
		{
			sb.Append("\t\tpublic static implicit operator ");
			sb.Append(baseTypeReference);
			sb.Append('(');
			sb.Append(typeNameAndGenerics);
			sb.Append(" obj) => obj.");
			sb.Append(baseTypeName);
			sb.AppendLine(";");
		}
		static void TypeDeclaration_Create_Base_IComposedOf(StringBuilder sb, WriteTypeParameterDelegate writeTypeParameter, ITypeSymbol baseTypeSymbol, string baseTypeName)
		{
			sb.Append("\t\t");
			TypeDeclaration_Create_Base_WriteType(sb, writeTypeParameter, baseTypeSymbol);
			sb.Append(" IComposedOf<");
			TypeDeclaration_Create_Base_WriteType(sb, writeTypeParameter, baseTypeSymbol);
			sb.Append(">.Base => this.");
			sb.Append(baseTypeName);
			sb.AppendLine(";");
		}
		static void TypeDeclaration_Create_Base_IEquatable(StringBuilder sb, CompositionBase compositionBase, string baseTypeName, string baseTypeReference)
		{
			sb.Append("\t\tbool IEquatable<");
			sb.Append(baseTypeReference);
			sb.Append(">.Equals(");
			sb.Append(baseTypeReference);
			if (compositionBase.BaseTypeSymbol.TypeKind != TypeKind.Struct)
				sb.Append('?');
			sb.Append(" obj) => this.");
			sb.Append(baseTypeName);
			sb.AppendLine(".Equals(obj);");
		}
		static void TypeDeclaration_Create_Base_Constructor(StringBuilder sb, CompositionBase compositionBase, string baseTypeName, string baseTypeReference, IMethodSymbol constructor)
		{
			if (constructor.IsImplicitlyDeclared) return;
			if (constructor.IsStatic) return;

			sb.Append("\t\t[MemberNotNull(nameof(");
			sb.Append(baseTypeName);
			sb.AppendLine("))]");

			sb.Append("\t\tpublic void Base");
			sb.Append(baseTypeName);
			sb.Append('(');
			bool first = true;
			foreach (var parameter in constructor.Parameters)
			{
				if (first) first = false; else sb.Append(", ");

				TypeDeclaration_Create_Base_WriteType(sb, (sb, tps) => TypeDeclaration_Create_Base_WriteType_GenericParameter(sb, tps, compositionBase), parameter.Type);
				sb.Append(' ');
				TypeDeclaration_Create_Base_WriteVariable(sb, parameter.Name);
			}
			sb.Append(") => this.");
			sb.Append(baseTypeName);
			sb.Append(" = new ");
			sb.Append(baseTypeReference);
			sb.Append('(');
			first = true;
			foreach (var parameter in constructor.Parameters)
			{
				if (first) first = false; else sb.Append(", ");
				TypeDeclaration_Create_Base_WriteVariable(sb, parameter.Name);
			}
			sb.AppendLine(");");
		}
		#region members
		static void TypeDeclaration_Create_Base_MembersRecursive(GeneratorExecutionContext context, TypeDeclarationSyntax typeDeclaration, TypePrefixes typePrefixes, StringBuilder sb, List<ISymbol> emittedSymbols, WriteTypeParameterDelegate writeTypeParameter, ITypeSymbol baseTypeSymbol, string baseTypeName, string baseTypeReference)
		{
			sb.Append("\t\t// Members: ");
			sb.AppendLine(baseTypeSymbol.Name);

			var members = baseTypeSymbol.GetMembers();
			var typeSymbol =
				TypeDeclaration_GetTypeSymbol(
					context, typePrefixes,
					SyntaxFactory.ParseTypeName(
						typeDeclaration.Identifier.ToFullString().Trim() +
						typeDeclaration.TypeParameterList?.ToFullString().Trim()
					)
				)!;
			foreach (var member in members)
			{
				TypeDeclaration_Create_Base_Member(context, typeDeclaration, typePrefixes, sb, emittedSymbols, writeTypeParameter, baseTypeName, baseTypeReference, typeSymbol, member);
			}
		}
		static void TypeDeclaration_Create_Base_Member(GeneratorExecutionContext context, TypeDeclarationSyntax typeDeclaration, TypePrefixes typePrefixes, StringBuilder sb, List<ISymbol> emittedSymbols, WriteTypeParameterDelegate writeTypeParameter, string baseTypeName, string baseTypeReference, INamedTypeSymbol typeSymbol, ISymbol member)
		{
			if (member.DeclaredAccessibility != Accessibility.Public && member.DeclaredAccessibility != Accessibility.Internal)
				return;
			var existingMember = TypeDeclaration_Create_Base_Member_FindExisting(member, typeSymbol);
			if (existingMember != null) return;

			if (
				emittedSymbols
					.Any(s =>
						s.Name == member.Name &&
						(
							s is not IMethodSymbol methodSymbol1 || member is not IMethodSymbol methodSymbol2 ||
							string.Join(",", methodSymbol1.Parameters) == string.Join(",", methodSymbol2.Parameters)
						)
					)
			)
				return;
			emittedSymbols.Add(member);

			if (member is IPropertySymbol propertySymbol)
			{
				TypeDeclaration_Create_Base_Member_Property(context, typeDeclaration, typePrefixes, sb, writeTypeParameter, baseTypeName, baseTypeReference, member, propertySymbol);
			}
			else if (member is IMethodSymbol methodSymbol)
			{
				TypeDeclaration_Create_Base_Member_Method(context, typeDeclaration, typePrefixes, sb, writeTypeParameter, baseTypeName, baseTypeReference, member, methodSymbol);
			}
			else if (member is IFieldSymbol fieldSymbol)
			{
				TypeDeclaration_Create_Base_Member_Field(context, typeDeclaration, typePrefixes, sb, writeTypeParameter, baseTypeName, baseTypeReference, member, fieldSymbol);
			}
			else if (member is IEventSymbol eventSymbol)
			{
				TypeDeclaration_Create_Base_Member_Event(context, typeDeclaration, typePrefixes, sb, writeTypeParameter, baseTypeName, baseTypeReference, member, eventSymbol);
			}
			else
			{
				throw new NotImplementedException();
			}
		}
		static void TypeDeclaration_Create_Base_Member_Property(GeneratorExecutionContext context, TypeDeclarationSyntax typeDeclaration, TypePrefixes typePrefixes, StringBuilder sb, WriteTypeParameterDelegate writeTypeParameter, string baseTypeName, string baseTypeReference, ISymbol member, IPropertySymbol propertySymbol)
		{
			TypeDeclaration_Create_Base_Member_Accessibility(context, typeDeclaration, typePrefixes, sb, member);

			TypeDeclaration_Create_Base_WriteType(sb, writeTypeParameter, propertySymbol.Type);
			sb.Append(' ');
			sb.Append(member.Name);
			sb.Append(" {");

			if (!propertySymbol.IsWriteOnly)
			{
				sb.Append(" get => ");
				TypeDeclaration_Create_Base_Member_Reference(sb, baseTypeName, baseTypeReference, member);
				sb.Append(';');
			}

			if (!propertySymbol.IsReadOnly)
			{
				sb.Append(" set => ");
				TypeDeclaration_Create_Base_Member_Reference(sb, baseTypeName, baseTypeReference, member);
				sb.Append(" = value;");
			}

			sb.AppendLine(" }");
		}
		static void TypeDeclaration_Create_Base_Member_Method(GeneratorExecutionContext context, TypeDeclarationSyntax typeDeclaration, TypePrefixes typePrefixes,
			StringBuilder sb, WriteTypeParameterDelegate writeTypeParameter, string baseTypeName, string baseTypeReference, ISymbol member, IMethodSymbol methodSymbol)
		{
			if (methodSymbol.MethodKind == MethodKind.Constructor) return;

			if (
				member.Name == "Equals" &&
				methodSymbol.Parameters.Length == 1 &&
				methodSymbol.Parameters[0].Type.Name.Equals("object", StringComparison.CurrentCultureIgnoreCase)
			)
				return;

			if (member.Name == "GetHashCode" && methodSymbol.Parameters.Length == 0)
				return;

			if (member.Name.StartsWith("get_") || member.Name.StartsWith("set_") || member.Name.StartsWith("add_") || member.Name.StartsWith("remove_"))
				return;

			TypeDeclaration_Create_Base_Member_Accessibility(context, typeDeclaration, typePrefixes, sb, member);
			TypeDeclaration_Create_Base_WriteType(sb, writeTypeParameter, methodSymbol.ReturnType);
			sb.Append(' ');
			sb.Append(member.Name);
			sb.Append('(');
			bool first = true;
			foreach (var param in methodSymbol.Parameters)
			{
				if (first) first = false; else sb.Append(", ");
				TypeDeclaration_Create_Base_WriteType(sb, writeTypeParameter, param.Type);
				sb.Append(' ');
				TypeDeclaration_Create_Base_WriteVariable(sb, param.Name);
				if (param.HasExplicitDefaultValue)
				{
					sb.Append(" = ");
					if (param.ExplicitDefaultValue == null)
					{
						sb.Append("null");
					}
					else if (param.ExplicitDefaultValue is bool boolValue)
					{
						sb.Append(boolValue.ToString().ToLower());
					}
					else if (param.ExplicitDefaultValue is string strValue)
					{
						sb.Append(SyntaxFactory.Literal(strValue).ToFullString());
					}
					else
					{
						sb.Append(param.ExplicitDefaultValue);
					}
				}
			}
			sb.Append(") => ");
			TypeDeclaration_Create_Base_Member_Reference(sb, baseTypeName, baseTypeReference, member);
			sb.Append('(');
			first = true;
			foreach (var param in methodSymbol.Parameters)
			{
				if (first) first = false; else sb.Append(", ");
				TypeDeclaration_Create_Base_WriteVariable(sb, param.Name);
			}
			sb.AppendLine(");");
		}
		static void TypeDeclaration_Create_Base_Member_Field(GeneratorExecutionContext context, TypeDeclarationSyntax typeDeclaration, TypePrefixes typePrefixes, StringBuilder sb, WriteTypeParameterDelegate writeTypeParameter, string baseTypeName, string baseTypeReference, ISymbol member, IFieldSymbol fieldSymbol)
		{
			// skip backing fields
			if (member.Name.Contains("<")) return;
			// skip private fields
			if (member.Name.StartsWith("_")) return;

			TypeDeclaration_Create_Base_Member_Accessibility(context, typeDeclaration, typePrefixes, sb, member);

			TypeDeclaration_Create_Base_WriteType(sb, writeTypeParameter, fieldSymbol.Type);
			sb.Append(' ');
			sb.Append(member.Name);
			sb.Append(" {");

			sb.Append(" get => ");
			TypeDeclaration_Create_Base_Member_Reference(sb, baseTypeName, baseTypeReference, member);
			sb.Append(';');

			if (!fieldSymbol.IsReadOnly)
			{
				sb.Append(" set => ");
				TypeDeclaration_Create_Base_Member_Reference(sb, baseTypeName, baseTypeReference, member);
				sb.Append(" = value;");
			}

			sb.AppendLine(" }");
		}
		static void TypeDeclaration_Create_Base_Member_Event(GeneratorExecutionContext context, TypeDeclarationSyntax typeDeclaration, TypePrefixes typePrefixes, StringBuilder sb, WriteTypeParameterDelegate writeTypeParameter, string baseTypeName, string baseTypeReference, ISymbol member, IEventSymbol eventSymbol)
		{
			TypeDeclaration_Create_Base_Member_Accessibility(context, typeDeclaration, typePrefixes, sb, member);
			sb.Append("event ");
			TypeDeclaration_Create_Base_WriteType(sb, writeTypeParameter, eventSymbol.Type);
			sb.Append(' ');
			sb.Append(member.Name);
			sb.Append(" { add => ");
			TypeDeclaration_Create_Base_Member_Reference(sb, baseTypeName, baseTypeReference, member);
			sb.AppendLine(" += value; remove => ");
			TypeDeclaration_Create_Base_Member_Reference(sb, baseTypeName, baseTypeReference, member);
			sb.AppendLine(" -= value; }");
		}
		static void TypeDeclaration_Create_Base_Member_Accessibility(GeneratorExecutionContext context, TypeDeclarationSyntax typeDeclaration, TypePrefixes typePrefixes, StringBuilder sb, ISymbol member)
		{
			sb.Append("\t\t");
			if (member.IsStatic)
			{
				WriteCompositionAccessibility(sb, member);
				sb.Append("static ");
			}
			else if (!typeDeclaration.Keyword.IsKind(SyntaxKind.StructKeyword))
			{
				ISymbol? foundBaseMember = null;
				if (typeDeclaration.BaseList != null)
				{
					var baseTypeSyntax = typeDeclaration.BaseList.Types[0];
					var baseTypeSymbol = TypeDeclaration_GetTypeSymbol(context, typePrefixes, baseTypeSyntax.Type);
					while (baseTypeSymbol != null && baseTypeSymbol.TypeKind != TypeKind.Interface)
					{
						foundBaseMember = TypeDeclaration_Create_Base_Member_FindExisting(member, baseTypeSymbol);
						if (foundBaseMember != null) break;
						baseTypeSymbol = baseTypeSymbol.BaseType;
					}
				}
				if (foundBaseMember != null)
				{
					if (foundBaseMember.IsVirtual)
					{
						sb.Append(SyntaxFacts.GetText(foundBaseMember.DeclaredAccessibility));
						sb.Append(' ');
						sb.Append("override ");
					}
					else
					{
						WriteCompositionAccessibility(sb, member);
						sb.Append("new ");
					}
				}
				else
				{
					WriteCompositionAccessibility(sb, member);
					sb.Append("virtual ");
				}
			}
			else
			{
				WriteCompositionAccessibility(sb, member);
			}

			static void WriteCompositionAccessibility(StringBuilder sb, ISymbol member)
			{
				sb.Append(SyntaxFacts.GetText(member.DeclaredAccessibility));
				sb.Append(' ');
			}
		}
		static ISymbol? TypeDeclaration_Create_Base_Member_FindExisting(ISymbol member, INamedTypeSymbol baseTypeSymbol)
		{
			var baseMembers = baseTypeSymbol.GetMembers(member.Name);
			foreach (var baseMember in baseMembers)
			{
				if (baseMember.Kind != member.Kind) continue;
				if (member is IMethodSymbol methodSymbol)
				{
					if (baseMember is not IMethodSymbol baseMethodSymbol) continue;
					if (methodSymbol.Parameters.Length != baseMethodSymbol.Parameters.Length) continue;
					bool parametersMatch = true;
					for (int i = 0; i < methodSymbol.Parameters.Length; ++i)
					{
						if (methodSymbol.Parameters[i].Type.ToDisplayString() != baseMethodSymbol.Parameters[i].Type.ToDisplayString())
						{
							parametersMatch = false;
							break;
						}
					}
					if (!parametersMatch) continue;
				}

				return baseMember;
			}

			return null;
		}
		static void TypeDeclaration_Create_Base_Member_Reference(StringBuilder sb, string baseTypeName, string baseTypeReference, ISymbol member)
		{
			if (member.IsStatic)
			{
				sb.Append(baseTypeReference);
			}
			else
			{
				sb.Append("this.");
				sb.Append(baseTypeName);
			}
			sb.Append('.');
			sb.Append(member.Name);
		}
		#endregion
		static void TypeDeclaration_Create_Base_WriteType(StringBuilder sb, WriteTypeParameterDelegate writeTypeParameter, ITypeSymbol type)
		{
			if (type is ITypeParameterSymbol typeParameter)
			{
				writeTypeParameter(sb, typeParameter);
			}
			else if (type is INamedTypeSymbol namedTypeSymbol)
			{
				switch (namedTypeSymbol.ContainingNamespace?.Name + "." + namedTypeSymbol.Name)
				{
					case "System.Void": sb.Append("void"); break;
					case "System.String": sb.Append("string"); break;
					case "System.Int32": sb.Append("int"); break;
					case "System.Int64": sb.Append("long"); break;
					case "System.Int16": sb.Append("short"); break;
					case "System.Byte": sb.Append("byte"); break;
					case "System.UInt32": sb.Append("uint"); break;
					case "System.UInt64": sb.Append("ulong"); break;
					case "System.UInt16": sb.Append("ushort"); break;
					case "System.SByte": sb.Append("sbyte"); break;
					case "System.Single": sb.Append("float"); break;
					case "System.Double": sb.Append("double"); break;
					case "System.Nullable":
						TypeDeclaration_Create_Base_WriteType(sb, writeTypeParameter, namedTypeSymbol.TypeArguments[0]);
						break;
					default:
						void WriteNamespaceRecursive(ISymbol symbol)
						{
							if (symbol.ContainingNamespace.Name == "") return;
							WriteNamespaceRecursive(symbol.ContainingNamespace);
							sb.Append(symbol.ContainingNamespace.Name);
							sb.Append('.');
						}
						if (namedTypeSymbol.ContainingSymbol is INamespaceSymbol namespaceSymbol)
							WriteNamespaceRecursive(namedTypeSymbol);
						else if (namedTypeSymbol.ContainingSymbol is ITypeSymbol containingTypeSymbol)
						{
							TypeDeclaration_Create_Base_WriteType(sb, writeTypeParameter, containingTypeSymbol);
							sb.Append('.');
						}
						sb.Append(namedTypeSymbol.Name);
						if (namedTypeSymbol.IsGenericType)
						{
							sb.Append('<');
							bool first = true;
							foreach (var typeArgument in namedTypeSymbol.TypeArguments)
							{
								if (first) first = false; else sb.Append(", ");
								TypeDeclaration_Create_Base_WriteType(sb, writeTypeParameter, typeArgument);
							}
							sb.Append('>');
						}
						break;
				}
			}
			else if (type is IArrayTypeSymbol arrayType)
			{
				TypeDeclaration_Create_Base_WriteType(sb, writeTypeParameter, arrayType.ElementType);
				sb.Append("[]");
			}
			else
			{
				throw new NotImplementedException();
			}
			if (type.NullableAnnotation == NullableAnnotation.Annotated)
				sb.Append('?');
		}
		static void TypeDeclaration_Create_Base_WriteType_GenericParameter(StringBuilder sb, ITypeParameterSymbol typeParameterSymbol, CompositionBase compositionBase)
		{
			int index = compositionBase.BaseTypeSymbol.TypeParameters.IndexOf(typeParameterSymbol);
			if (index != -1)
			{
				sb.Append(((GenericNameSyntax)compositionBase.BaseTypeSyntax).TypeArgumentList.Arguments[index].ToFullString());
			}
			else
			{
				sb.Append(typeParameterSymbol.Name);
			}
		}
		static void TypeDeclaration_Create_Base_WriteType_GenericParameter(StringBuilder sb, ITypeParameterSymbol typeParameterSymbol, CompositionBaseNextBase compositionBaseNextBase)
		{
			int index = compositionBaseNextBase.BaseTypeSymbol.TypeParameters.IndexOf(typeParameterSymbol);
			if (index != -1)
			{
				string substituteName = ((GenericNameSyntax)compositionBaseNextBase.BaseTypeSyntax).TypeArgumentList.Arguments[index].ToFullString();
				object parent = compositionBaseNextBase.Parent;
				while (parent is CompositionBaseNextBase parentNextBase)
				{
					index = TypeDeclaration_Create_Base_WriteType_GenericParameter_FindTypeArg(substituteName, parentNextBase.BaseTypeSymbol);
					if (index != -1)
					{
						substituteName = ((GenericNameSyntax)parentNextBase.BaseTypeSyntax).TypeArgumentList.Arguments[index].ToFullString();
					}
					else
						goto done;
					parent = parentNextBase.Parent;
				}
				var compositionBase = (CompositionBase)parent;
				index = TypeDeclaration_Create_Base_WriteType_GenericParameter_FindTypeArg(substituteName, compositionBase.BaseTypeSymbol);
				if (index != -1)
				{
					substituteName = ((GenericNameSyntax)compositionBase.BaseTypeSyntax).TypeArgumentList.Arguments[index].ToFullString();
				}

			done:
				sb.Append(substituteName);
			}
			else
			{
				sb.Append(typeParameterSymbol.Name);
			}
		}
		static int TypeDeclaration_Create_Base_WriteType_GenericParameter_FindTypeArg(string substituteName, INamedTypeSymbol baseTypeSymbol)
		{
			int index = -1;
			for (int i = 0; i < baseTypeSymbol.TypeParameters.Length; ++i)
			{
				if (baseTypeSymbol.TypeParameters[i].Name == substituteName)
				{
					index = i;
					break;
				}
			}

			return index;
		}
		static void TypeDeclaration_Create_Base_WriteVariable(StringBuilder sb, string name)
		{
			if (name == "this")
				sb.Append('@');
			sb.Append(name);
		}
		#endregion
		#region equals object
		static void TypeDeclaration_Create_EqualsObject(GeneratorExecutionContext context, TypeDeclarationSyntax typeDeclaration, TypePrefixes typePrefixes, List<CompositionBase> compositionBases, StringBuilder sb)
		{
			sb.Append("\t\tpublic ");
			bool hasBase = false;
			if (typeDeclaration.Keyword.IsKind(SyntaxKind.ClassKeyword))
			{
				if (typeDeclaration.BaseList != null)
				{
					var baseTypeSyntax = typeDeclaration.BaseList.Types[0];
					var baseTypeSymbol = TypeDeclaration_GetTypeSymbol(context, typePrefixes, baseTypeSyntax.Type);
					while (true)
					{
						if (baseTypeSymbol == null) break;

						if (
							baseTypeSymbol.TypeKind != TypeKind.Interface &&
							baseTypeSymbol.GetAttributes().Any(ad => ad.AttributeClass?.Name == "ComposedOf" || ad.AttributeClass?.Name == "ComposedOfAttribute")
						)
						{
							hasBase = true;
							sb.Append("override");
							break;
						}

						baseTypeSymbol = baseTypeSymbol.BaseType;
					}
				}
				if (!hasBase)
				{
					sb.Append("virtual");
				}
			}
			sb.AppendLine(" bool BaseEquals(object? obj)");
			sb.AppendLine("\t\t{");
			foreach (var compositionBase in compositionBases)
			{
				var baseTypeSymbol = compositionBase.BaseTypeSymbol;
				TypeDeclaration_Create_EqualsObject_BaseType(sb, (sb, tps) => TypeDeclaration_Create_Base_WriteType_GenericParameter(sb, tps, compositionBase), "", baseTypeSymbol);
				foreach (var nextBase in compositionBase.NextBases)
				{
					TypeDeclaration_Create_EqualsObject_BaseType(sb, (sb, tps) => TypeDeclaration_Create_Base_WriteType_GenericParameter(sb, tps, nextBase), nextBase.FieldPrefix, nextBase.BaseTypeSymbol);
				}
			}

			sb.Append("\t\t\treturn base.");
			if (hasBase)
			{
				sb.Append("BaseEquals");
			}
			else
			{
				sb.Append("Equals");
			}
			sb.AppendLine("(obj);");

			sb.AppendLine("\t\t}");
		}
		static void TypeDeclaration_Create_EqualsObject_BaseType(StringBuilder sb, WriteTypeParameterDelegate writeTypeParameter, string prefix, ITypeSymbol baseTypeSymbol)
		{
			string baseTypeName = baseTypeSymbol.Name;

			void WriteVariableName()
			{
				sb.Append(char.ToLower(baseTypeName[0]));
				sb.Append(baseTypeName.Substring(1));
				sb.Append("Local");
			}

			sb.Append("\t\t\tif (obj is ");
			TypeDeclaration_Create_Base_WriteType(sb, writeTypeParameter, baseTypeSymbol);
			sb.Append(' ');
			WriteVariableName();
			sb.AppendLine(")");
			sb.Append("\t\t\t\treturn this.");
			sb.Append(prefix);
			sb.Append(baseTypeName);
			sb.Append(".Equals(");
			WriteVariableName();
			sb.AppendLine(");");
		}
		#endregion
		#region composition part
		static void TypeDeclaration_Create_Part(GeneratorExecutionContext context, TypeDeclarationSyntax typeDeclaration, TypePrefixes typePrefixes, List<CompositionBase> compositionBases, string typeNameAndGenerics, StringBuilder sb)
		{
			sb.Append("\tpublic interface ");
			WriteCompositionInterfaceNameAndGenerics(sb, typeNameAndGenerics);
			sb.Append(" : IComposedOf<");
			sb.Append(typeNameAndGenerics);
			sb.Append('>');
			foreach (var compositionBase in compositionBases)
			{
				if (
					compositionBase.BaseTypeSymbol.GetAttributes()
						.Any(ad => ad.AttributeClass?.Name == "CompositionPart" || ad.AttributeClass?.Name == "CompositionPartAttribute")
				)
				{
					string baseTypeReference = compositionBase.BaseTypeSyntax.ToFullString();
					sb.Append(", ");
					WriteCompositionInterfaceReference(sb, baseTypeReference);
				}
				else
				{
				}
			}
			var baseInterfaces = new List<BaseInterfaceMembers>();
			if (typeDeclaration.BaseList != null)
			{
				foreach (var baseType in typeDeclaration.BaseList.Types)
				{
					var baseTypeSymbol = TypeDeclaration_GetTypeSymbol(context, typePrefixes, baseType.Type);
					if (baseTypeSymbol != null && baseTypeSymbol.TypeKind == TypeKind.Interface)
					{
						sb.Append(", ");
						sb.Append(baseType.Type.ToFullString().Trim());
						baseInterfaces.Add(new BaseInterfaceMembers(baseType.Type, baseTypeSymbol, baseTypeSymbol.GetMembers()));
					}
				}
			}
			sb.AppendLine();

			if (typeDeclaration.ConstraintClauses.Count > 0)
			{
				sb.Append(typeDeclaration.ConstraintClauses.ToFullString());
			}

			sb.AppendLine("\t{");

			var typeSymbol =
				context.Compilation.GetTypeByMetadataName(
					typePrefixes.NamePrefix + typeDeclaration.Identifier.Text + (typeDeclaration.Arity > 0 ? "`" + typeDeclaration.Arity : "")
				)!;
			var members = typeSymbol.GetMembers();
			foreach (var member in members)
			{
				TypeDeclaration_Create_Part_Member(sb, baseInterfaces, member);
			}

			sb.AppendLine("\t}");
		}
		static void TypeDeclaration_Create_Part_Member(StringBuilder sb, List<BaseInterfaceMembers> baseInterfaces, ISymbol member)
		{
			if (member.DeclaredAccessibility != Accessibility.Public) return;
			if (member.IsStatic) return;
			foreach (var baseInterface in baseInterfaces)
			{
				foreach (var baseMember in baseInterface.Members)
				{
					if (baseMember.Name != member.Name) continue;
					if (baseMember.Kind != member.Kind) continue;
					if (member is IMethodSymbol methodSymbol && baseMember is IMethodSymbol baseMethodSymbol)
					{
						if (methodSymbol.Parameters.Length != baseMethodSymbol.Parameters.Length) continue;
					}
					return;
				}
			}

			if (member is IPropertySymbol propertySymbol)
			{
				TypeDeclaration_Create_Part_Member_Property(sb, member, propertySymbol);
			}
			else if (member is IMethodSymbol methodSymbol)
			{
				TypeDeclaration_Create_Part_Member_Method(sb, member, methodSymbol);
			}
			else if (member is IFieldSymbol fieldSymbol)
			{
				TypeDeclaration_Create_Part_Member_Field(sb, member, fieldSymbol);
			}
			else if (member is IEventSymbol eventSymbol)
			{
				TypeDeclaration_Create_Part_Member_Event(sb, member, eventSymbol);
			}
			else
			{
				throw new NotImplementedException();
			}
		}
		static void TypeDeclaration_Create_Part_Member_Property(StringBuilder sb, ISymbol member, IPropertySymbol propertySymbol)
		{
			sb.Append("\t\t");

			sb.Append($@"{propertySymbol.Type.ToDisplayString()} {member.Name} {{");
			if (!propertySymbol.IsWriteOnly)
			{
				sb.Append($@" get;");
			}
			if (!propertySymbol.IsReadOnly)
			{
				sb.Append($@" set;");
			}
			sb.AppendLine(" }");
		}
		static void TypeDeclaration_Create_Part_Member_Method(StringBuilder sb, ISymbol member, IMethodSymbol methodSymbol)
		{
			if (methodSymbol.MethodKind == MethodKind.Constructor || methodSymbol.MethodKind == MethodKind.SharedConstructor) return;

			if (member.Name.StartsWith("get_") || member.Name.StartsWith("set_") || member.Name.StartsWith("add_") || member.Name.StartsWith("remove_"))
				return;

			var methodSyntax = (MethodDeclarationSyntax)member.DeclaringSyntaxReferences[0].SyntaxTree.GetRoot().FindNode(member.DeclaringSyntaxReferences[0].Span);

			if (
				member.Name == "Equals" &&
				methodSyntax.ParameterList.Parameters.Count == 1 &&
				(
					methodSyntax.ParameterList.Parameters[0].Type!.ToFullString().Trim().Equals("object", StringComparison.CurrentCultureIgnoreCase) ||
					methodSyntax.ParameterList.Parameters[0].Type!.ToFullString().Trim().Equals("object?", StringComparison.CurrentCultureIgnoreCase)
				)
			)
				return;

			if (member.Name == "GetHashCode" && methodSyntax.ParameterList.Parameters.Count == 0)
				return;

			sb.Append("\t\t");
			sb.Append(methodSyntax.ReturnType.ToFullString());
			sb.Append(member.Name);
			sb.Append(methodSyntax.ParameterList.ToFullString().Trim());
			sb.AppendLine(";");
		}
		static void TypeDeclaration_Create_Part_Member_Field(StringBuilder sb, ISymbol member, IFieldSymbol fieldSymbol)
		{
			// skip backing fields
			if (member.Name.Contains("<")) return;
			// skip private fields
			if (member.Name.StartsWith("_")) return;
			// skip statics
			if (member.IsStatic) return;

			sb.Append("\t\t");
			sb.Append($@"{fieldSymbol.Type.ToDisplayString()} {member.Name} {{ get; set;");
			sb.AppendLine(" }");
		}
		static void TypeDeclaration_Create_Part_Member_Event(StringBuilder sb, ISymbol member, IEventSymbol eventSymbol)
		{
			sb.Append("\t\t");
			sb.AppendLine($@"event {eventSymbol.Type.ToDisplayString()} {member.Name};");
		}
		#endregion
	}
}
