using System;

[AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = true)]
public class ComposedOfAttribute : Attribute
{
	public ComposedOfAttribute(Type type)
	{
		this.Type = type;
	}
	public ComposedOfAttribute(Type type, string genericsString)
	{
		this.Type = type;
		this.GenericsString = genericsString;
	}

	public Type Type { get; }
	public string? GenericsString { get; }
}
