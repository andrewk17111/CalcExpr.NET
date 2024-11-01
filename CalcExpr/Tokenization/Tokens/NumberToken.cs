using System.Text.RegularExpressions;

namespace CalcExpr.Tokenization.Tokens;

public partial class NumberToken(string value, int index) : IToken
{
    public string Value { get; } = value;

    public double ParsedValue { get; } = double.Parse(value);

    public int Index { get; } = index;

    public override bool Equals(object? obj)
        => obj is NumberToken token && token.Value == Value;

    public override int GetHashCode()
        => Value.GetHashCode();

    public static bool operator ==(NumberToken a, IToken b)
        => a.Equals(b);

    public static bool operator !=(NumberToken a, IToken b)
        => !a.Equals(b);

    public static implicit operator NumberToken(Match match)
        => new NumberToken(match.Value, match.Index);

    public static implicit operator string(NumberToken token)
        => token.Value;
}
