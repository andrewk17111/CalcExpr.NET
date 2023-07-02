using System.Text.RegularExpressions;

namespace CalcExpr.Parsing;

public readonly struct Token
{
    public readonly string Value;
    public readonly int Index;
    public readonly int Length;

    public Token(string value, int index, int length)
    {
        Value = value;
        Index = index;
        Length = length;
    }

    public static implicit operator Token(Match match)
        => new Token(match.Value, match.Index, match.Length);
}
