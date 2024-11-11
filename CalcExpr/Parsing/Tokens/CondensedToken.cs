using CalcExpr.Tokenization.Tokens;

namespace CalcExpr.Parsing.Tokens;

public class CondensedToken(List<IToken> value, int index, int tokenIndex) : IToken
{
    private readonly List<IToken> _value = value;

    public string Value { get; } = $"[{tokenIndex}]";

    public int Index { get; } = index;

    public int TokenIndex { get; } = tokenIndex;

    public List<IToken> Tokens => [.. _value];

    public char RegexAlias => '\u001A';
}
