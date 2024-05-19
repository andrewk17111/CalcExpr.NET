namespace DiceEngine.Attributes;

public class BuiltInFunctionAttribute : Attribute
{
    public string[] Aliases { get; }

    public BuiltInFunctionAttribute(params string[] aliases)
    {
        Aliases = aliases;
    }
}
