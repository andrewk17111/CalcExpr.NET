﻿using System.Diagnostics.CodeAnalysis;

namespace CalcExpr.Tokenization.Tokens;

/// <summary>
/// A substring that represents an opening bracket.
/// </summary>
/// <param name="bracketType">The type of bracket.</param>
/// <param name="index">The starting index within the containing <see cref="string"/>.</param>
public partial class OpenBracketToken(Bracket bracketType, int index) : IToken
{
    public string Value { get; } = bracketType switch
    {
        Bracket.Parenthesis => "(",
        Bracket.Square => "[",
        Bracket.Curly => "{",
        Bracket.Angle => "<",
        _ => throw new NotSupportedException()
    };

    public int Index { get; } = index;

    public Bracket BracketType { get; } = bracketType;

    public char RegexAlias => Value.First();

    public override bool Equals(object? obj)
        => obj is OpenBracketToken token && token.Value == Value;

    public override int GetHashCode()
        => Value.GetHashCode();

    public static bool operator ==(OpenBracketToken a, IToken b)
        => a.Equals(b);

    public static bool operator !=(OpenBracketToken a, IToken b)
        => !a.Equals(b);

    public static implicit operator string(OpenBracketToken token)
        => token.Value;
}

/// <summary>
/// A substring that represents a closing bracket.
/// </summary>
/// <param name="bracketType">The type of bracket.</param>
/// <param name="index">The starting index within the containing <see cref="string"/>.</param>
public partial class CloseBracketToken(Bracket bracketType, int index) : IToken
{
    public string Value { get; } = bracketType switch
    {
        Bracket.Parenthesis => ")",
        Bracket.Square => "]",
        Bracket.Curly => "}",
        Bracket.Angle => ">",
        _ => throw new NotSupportedException()
    };

    public int Index { get; } = index;

    public Bracket BracketType { get; } = bracketType;

    public char RegexAlias => Value.First();

    public override bool Equals([NotNullWhen(true)] object? obj)
        => obj is CloseBracketToken token && token.Value == Value;

    public override int GetHashCode()
        => Value.GetHashCode();

    public static bool operator ==(CloseBracketToken a, IToken b)
        => a.Equals(b);

    public static bool operator !=(CloseBracketToken a, IToken b)
        => !a.Equals(b);

    public static implicit operator string(CloseBracketToken token)
        => token.Value;
}

public enum Bracket
{
    Parenthesis = 1,
    Square = 2,
    Curly = 4,
    Angle = 8,
}
