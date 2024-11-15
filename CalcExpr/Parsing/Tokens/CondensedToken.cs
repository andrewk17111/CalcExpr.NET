using CalcExpr.Tokenization.Tokens;
using System.Collections.Immutable;

namespace CalcExpr.Parsing.Tokens;

public class CondensedToken(ImmutableArray<IToken> value, int index, int tokenIndex) : IToken
{
    private readonly ImmutableArray<IToken> _value = value;

    public string Value { get; } = $"[{tokenIndex}]";

    public int Index { get; } = index;

    public int TokenIndex { get; } = tokenIndex;

    public ImmutableArray<IToken> Tokens => [.. _value];

    public char RegexAlias => '\u001A';
}
