using CalcExpr.Tokenization.Tokens;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;

namespace CalcExpr.Parsing.Tokens;

public class CondensedToken(ImmutableArray<IToken> value, int index, int tokenIndex) : IToken
{
    private readonly ImmutableArray<IToken> _value = value;

    public string Value { get; } = $"[{tokenIndex}]";

    public int Index { get; } = index;

    public int TokenIndex { get; } = tokenIndex;

    public ImmutableArray<IToken> Tokens => [.. _value];

    public char RegexAlias => '\u001A';

    public override bool Equals([NotNullWhen(true)] object? obj)
        => obj is CondensedToken token && token._value.SequenceEqual(_value);

    public override int GetHashCode()
        => Value.GetHashCode();

    public static bool operator ==(CondensedToken a, IToken b)
        => a.Equals(b);

    public static bool operator !=(CondensedToken a, IToken b)
        => !a.Equals(b);
}
