using CalcExpr.Extensions;
using CalcExpr.Tokenization.Tokens;
using System.Collections;

namespace CalcExpr.Parsing;

public class TokenMatch(IEnumerable<IToken> match, int index) : IEnumerable<IToken>
{
    private readonly IToken[] _match = [.. match];

    public IReadOnlyList<IToken> Match => _match;

    public int Index { get; } = index;

    public int Length => Match.Count;

    public string Value => _match.ToList().JoinTokens();

    public IToken this[int index] => _match[index];

    public List<IToken> this[Range range] => [.. _match[range]];

    public IEnumerator<IToken> GetEnumerator() => ((IEnumerable<IToken>)_match).GetEnumerator();

    IEnumerator IEnumerable.GetEnumerator() => _match.GetEnumerator();
}
