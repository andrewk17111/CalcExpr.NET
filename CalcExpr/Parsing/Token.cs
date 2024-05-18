using System.Diagnostics.CodeAnalysis;
using System.Text.RegularExpressions;

namespace CalcExpr.Parsing;

/// <summary>
/// Creates and initializes a new <see cref="Token"/> with the specified <paramref name="value"/>, 
/// <paramref name="index"/>, and <paramref name="index"/>.
/// </summary>
/// <param name="value">The <see cref="string"/> contained within the <see cref="Token"/>.</param>
/// <param name="index">The starting index within the containing <see cref="string"/>.</param>
public readonly struct Token(string value, int index)
{
    public readonly string Value = value;
    public readonly int Index = index;

    public int Length
        => Value.Length;

    public char this[int index]
        => Value[index];

    public string this[Range range]
        => Value[range];

    public static implicit operator Token(Match match)
        => new Token(match.Value, match.Index);

    public static implicit operator string(Token token)
        => token.Value;

    public override bool Equals([NotNullWhen(true)] object? obj)
        => obj is not null && obj is Token token && token.Value == Value;

    public override int GetHashCode()
        => Value.GetHashCode();

    public static bool operator ==(Token a, Token b)
        => a.Equals(b);

    public static bool operator !=(Token a, Token b)
        => !a.Equals(b);
}
