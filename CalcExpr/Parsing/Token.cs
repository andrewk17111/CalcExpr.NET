using System.Text.RegularExpressions;

namespace CalcExpr.Parsing;

public readonly struct Token
{
    public readonly string Value;
    public readonly int Index;
    public readonly int Length;

    /// <summary>
    /// Creates and initializes a new <see cref="Token"/> with the specified <paramref name="value"/>, 
    /// <paramref name="index"/>, and <paramref name="index"/>.
    /// </summary>
    /// <param name="value">The <see cref="string"/> contained within the <see cref="Token"/>.</param>
    /// <param name="index">The starting index within the containing <see cref="string"/>.</param>
    /// <param name="length">The length of <paramref name="value"/>.</param>
    public Token(string value, int index, int length)
    {
        Value = value;
        Index = index;
        Length = length;
    }

    public static implicit operator Token(Match match)
        => new Token(match.Value, match.Index, match.Length);
}
