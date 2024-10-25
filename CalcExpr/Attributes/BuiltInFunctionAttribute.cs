namespace CalcExpr.Attributes;

public class NativeFunctionAttribute : Attribute
{
    public string[] Aliases { get; }

    public NativeFunctionAttribute(params string[] aliases)
    {
        Aliases = aliases;
    }
}
