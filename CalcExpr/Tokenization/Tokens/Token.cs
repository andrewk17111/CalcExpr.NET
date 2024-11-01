using CalcExpr.Tokenization.Rules;
using System.Text.RegularExpressions;

namespace CalcExpr.Tokenization.Tokens;

/// <summary>
/// A substring that matches a <see cref="ITokenizerRule"/>.
/// </summary>
/// <param name="value">The <see cref="string"/> contained within the <see cref="Token"/>.</param>
/// <param name="index">The starting index within the containing <see cref="string"/>.</param>
public class Token(string value, int index) : IToken
{
    public string Value { get; } = value;

    public int Index { get; } = index;

    public int Length => Value.Length;

    public char this[int index] => Value[index];

    public string this[Range range] => Value[range];

    public override bool Equals(object? obj)
        => obj is Token token && token.Value == Value;

    public override int GetHashCode()
        => Value.GetHashCode();

    public static bool operator ==(Token a, IToken b)
        => a.Equals(b);

    public static bool operator !=(Token a, IToken b)
        => !a.Equals(b);

    public static implicit operator Token(Match match)
        => new Token(match.Value, match.Index);

    public static implicit operator string(Token token)
        => token.Value;
}
