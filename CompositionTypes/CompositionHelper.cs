using System;

public static class CompositionHelper
{
	public static T Unbox<T>(object obj)
	{
		if (obj is T res) return res;
		if (obj is IComposedOf<T> promotes) return promotes.Base;
		throw new InvalidCastException("Unable to unbox value of type '" + obj.GetType() + "' to type '" + typeof(T) + "'.");
	}
	public static T? TryUnbox<T>(object obj)
		where T : class
	{
		if (obj is T res) return res;
		if (obj is IComposedOf<T> promotes) return promotes.Base;
		return null;
	}
	public static T? TryUnboxStruct<T>(object obj)
		where T : struct
	{
		if (obj is T res) return res;
		if (obj is IComposedOf<T> promotes) return promotes.Base;
		return null;
	}
}
