using System.Text.RegularExpressions;

namespace CalcExpr.Parsing;

public readonly struct Token
{
    public readonly string Value;
    public readonly int Index;

    public int Length
        => Value.Length;

    /// <summary>
    /// Creates and initializes a new <see cref="Token"/> with the specified <paramref name="value"/>, 
    /// <paramref name="index"/>, and <paramref name="index"/>.
    /// </summary>
    /// <param name="value">The <see cref="string"/> contained within the <see cref="Token"/>.</param>
    /// <param name="index">The starting index within the containing <see cref="string"/>.</param>
    public Token(string value, int index)
    {
        Value = value;
        Index = index;
    }

    public static implicit operator Token(Match match)
        => new Token(match.Value, match.Index);

    public static implicit operator string(Token token)
        => token.Value;
}
