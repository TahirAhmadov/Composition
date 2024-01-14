# Composition

This source generator library aims to introduce a limited version of "multiple inheritance" and "struct inheritance" by using composition.

## How to install

Add the following to your SDK-style project file:
```xml
	<ItemGroup>
		<PackageReference Include="TA.Util.Composition" Version="1.1.0" OutputItemType="Analyzer" ReferenceOutputAssembly="false" />
		<PackageReference Include="TA.Util.CompositionTypes" Version="1.1.0" />
	</ItemGroup>
```

Keep in mind that since this is `ISourceGenerator`-based, VS performance may be negatively impacted if there are many projects; it's highly not recommended to apply this using `Directory.Build.props` or any other similar mechanism; 
it should only be added to the projects where it's needed.

- https://www.nuget.org/packages/TA.Util.CompositionTypes
- https://www.nuget.org/packages/TA.Util.Composition

## Usage

Imagine you have a struct like this:
```cs
[CompositionPart]
public struct Point2D : IEquatable<Point2D>
{
  public Point2D(double x, double y)
  {
    this.X = x;
    this.Y = y;
  }
  public double X, Y;

  public bool Equals(Point2D other) => this.X == other.X && this.Y == other.Y;
  public Point2D Add(Point2D obj) => new(this.X + obj.X, this.Y + obj.Y);
  public override bool Equals(object? obj) => obj is Point2D p && this.Equals(p);

  public override int GetHashCode() => this.X.GetHashCode() ^ this.Y.GetHashCode();
}
```

Note the optional `[CompositionPart]` attribute - it triggers the generation of an interface like below:
```cs
public interface IPoint2DComposition : IComposedOf<Point2D>, IEquatable<Point2D>
{
  double X { get; set; }
  double Y { get; set; }
  Point2D Add(Point2D obj);
}
```

Then, if you want to "inherit" `Point2D`:
```cs
[ComposedOf(typeof(Point2D))] 
public partial struct Point3D : IEquatable<Point3D>
{
  public Point3D(double x, double y, double z)
  {
    this.BasePoint2D(x, y);
    this.Z = z;
  }
  public double Z;
  public bool Equals(Point3D obj) => this.Point2D.Equals(obj.Point2D) && this.Z == obj.Z;
}
```

The `[ComposedOf(...)]` is the main attribute which is required to generate "compositions" like below:
```cs
#pragma warning disable CS0282 // There is no defined ordering between fields in multiple declarations of partial struct
partial struct Point3D : IEquatable<Point2D>, IPoint2DComposition
#pragma warning restore CS0282 // There is no defined ordering between fields in multiple declarations of partial struct
{
  // Base type: Point2D
  public Point2D Point2D;
  public static implicit operator Point2D(Point3D obj) => obj.Point2D;
  Library1.Point2D IComposedOf<Library1.Point2D>.Base => this.Point2D;
  bool IEquatable<Point2D>.Equals(Point2D obj) => this.Point2D.Equals(obj);
  // Constructors
  [MemberNotNull(nameof(Point2D))]
  public void BasePoint2D(double x, double y) => this.Point2D = new Point2D(x, y);
  // Members: Point2D
  public double X { get => this.Point2D.X; set => this.Point2D.X = value; }
  public double Y { get => this.Point2D.Y; set => this.Point2D.Y = value; }
  public System.Boolean Equals(Library1.Point2D other) => this.Point2D.Equals(other);
  public Library1.Point2D Add(Library1.Point2D obj) => this.Point2D.Add(obj);

  public  bool BaseEquals(object? obj)
  {
    if (obj is Library1.Point2D point2DLocal)
      return this.Point2D.Equals(point2DLocal);
    return base.Equals(obj);
  }
}
```

You are then able to do things like below:
```cs
var p3 = new Point3D(1, 2, 3);
Point2D p2 = p3; // p2.X == 1, p2.Y == 2; p3.Z was lost
//p3 = (Point3D)p2; // compile error - no conversion exists (unless defined manually)
double x = p3.X; // same as p3.Point2D.X
bool b = p3.Equals(p2); // same as p3.Point2D.Equals(p2) 
p2 = p3.Add(p2); // same as p3.Point2D.Add(p2); now p2.X = 2, p2.Y = 4
object p3Boxed = p3;
var p2Equatable = (IEquatable<Point2D>)p3Boxed; // works - because we "lifted" the interfaces
var p2Composition = (IPoint2DComposition)p3Boxed; // this interface is implemented, allowing us to invoke Point2D members
var p2Composition2 = (IComposedOf<Point2D>)p3Boxed; // if [CompositionPart] is not added to Point2D, then this generic interface can be used
```

Due to the limitation on generic arguments being saved as attribute parameters, an overload is available - `[ComposedOf(Type, string)]`:
```cs
public class Class2D<T2> { ..... }

[ComposedOf(typeof(Class2D<>), "<T3>")]
public partial class Class3DGeneric<T3>
{
  public Class3DGeneric(...)
  {
    this.BaseClass2D(...);
    // ....
  }
}
```
