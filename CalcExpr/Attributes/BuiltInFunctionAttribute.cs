namespace CalcExpr.Attributes;

public class BuiltInFunctionAttribute : Attribute
{
    public string Name { get; }

    public BuiltInFunctionAttribute(string name)
    {
        Name = name;
    }
}
