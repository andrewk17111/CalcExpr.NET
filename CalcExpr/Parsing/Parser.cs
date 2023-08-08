using CalcExpr.Expressions;
using System.Text.RegularExpressions;

namespace CalcExpr.Parsing;

public class Parser
{
    private readonly List<Rule> _grammar = new List<Rule>();

    public Rule[] Grammar
        => throw new NotImplementedException();

    public Parser()
    {
        _grammar = new List<Rule>()
        {
            new Rule(@"(?<=^\s*)[\+\-!~¬]", ParsePrefix),
            new Rule(@"[%!](?=\s*$)", ParsePostfix),
            new Rule(@"(?<=^\s*)((\d+\.?\d*)|(\d*\.?\d+))(?=\s*$)", ParseNumber)
        };
    }

    public Parser(IEnumerable<Rule> grammar)
        => _grammar = grammar.ToList();

    public IExpression Parse(string input)
    {
        if (input is null)
            throw new ArgumentNullException(nameof(input));

        string clean_input = Regex.Replace(input, @"\s+", " ");

        foreach (Rule rule in _grammar)
        {
            Match match = Regex.Match(input, rule.RegularExpression);

            if (match.Success)
                return rule.Parse.Invoke(input, match);
        }

        throw new Exception($"The input was not in the correct format: '{input}'");
    }

    /// <summary>
    /// Determines whether the cache of the <see cref="Parser"/> contains a specified expression <see cref="string"/>.
    /// </summary>
    /// <param name="expression">The expression to locate in the cache.</param>
    /// <returns>
    /// <see langword="true"/> if the cache contains an entry with the specified expression; otherwise, 
    /// <see langword="false"/>.
    /// </returns>
    public bool ContainsCache(string expression)
        => throw new NotImplementedException();

    /// <summary>
    /// Add expression <see cref="string"/> to the cache of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="key">The expression <see cref="string"/> of the cached <see cref="IExpression"/>.</param>
    /// <param name="value">The cached <see cref="IExpression"/>.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="IExpression"/> was successfully added to the cache; otherwise, 
    /// <see langword="false"/>.
    /// </returns>
    public bool AddCache(string key, IExpression value)
        => throw new NotImplementedException();

    /// <summary>
    /// Removes a cached <see cref="IExpression"/> based on the specified expression <see cref="string"/>.
    /// </summary>
    /// <param name="expression">The expression <see cref="string"/> of the cached <see cref="IExpression"/>.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="IExpression"/> was successfully removed from the cache; otherwise,
    /// <see langword="false"/>.
    /// </returns>
    public bool RemoveCache(string expression)
        => throw new NotImplementedException();

    /// <summary>
    /// Removes a cached <see cref="IExpression"/> based on the index in the cache.
    /// </summary>
    /// <param name="index">The index in the cache of the <see cref="IExpression"/>.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="IExpression"/> was successfully removed from the cache; otherwise,
    /// <see langword="false"/>.
    /// </returns>
    public bool RemoveCacheAt(int index)
        => throw new NotImplementedException();

    /// <summary>
    /// Add <see cref="Rule"/> to the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="rule">The <see cref="Rule"/> to be added to the grammar.</param>
    /// <param name="index">The index to put the <see cref="Rule"/> in the grammar.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully added to the grammar; otherwise, 
    /// <see langword="false"/>.
    /// </returns>
    public bool AddGrammarRule(Rule rule, int index = -1)
        => throw new NotImplementedException();

    /// <summary>
    /// Add <see cref="Rule"/> to the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="regex">The regular expression to match for the new <see cref="Rule"/>.</param>
    /// <param name="parse_func">
    /// The <see cref="Func{string, Token, IExpression}"/> of the new <see cref="Rule"/> to parse the matched 
    /// <see cref="Token"/>.
    /// </param>
    /// <param name="index">The index to put the <see cref="Rule"/> in the grammar.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully added to the grammar; otherwise, 
    /// <see langword="false"/>.
    /// </returns>
    public bool AddGrammarRule(string regex, Func<string, Token, IExpression> parse_func, int index = -1)
        => AddGrammarRule(new Rule(regex, parse_func), index);

    /// <summary>
    /// Removes the <see cref="Rule"/> with the specified regex <see cref="string"/> from the grammar of the 
    /// <see cref="Parser"/>.
    /// </summary>
    /// <param name="regex">The regex <see cref="string"/> for the <see cref="Rule"/> to be removed.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully removed; otherwise, <see langword="false"/>.
    /// </returns>
    public bool RemoveGrammarRule(string regex)
        => throw new NotImplementedException();

    /// <summary>
    /// Removes the <see cref="Rule"/> at the specified index from the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="index">The index for the <see cref="Rule"/> to be removed.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully removed; otherwise, <see langword="false"/>.
    /// </returns>
    public bool RemoveGrammarRuleAt(int index)
        => throw new NotImplementedException();

    /// <summary>
    /// Determines whether a rule with the specified regex <see cref="string"/> is in the grammar of the 
    /// <see cref="Parser"/>.
    /// </summary>
    /// <param name="regex">The regex <see cref="string"/> of the <see cref="Rule"/> to find.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully found; otherwise, <see langword="false"/>.
    /// </returns>
    public bool GrammarContains(string regex)
        => throw new NotImplementedException();

    private static IExpression ParseNumber(string input, Token match)
        => new Number(Convert.ToDouble(match.Value));

    private IExpression ParsePrefix(string input, Token match)
        => new UnaryOperator(match.Value, true, Parse(input[match.Length..]));

    private IExpression ParsePostfix(string input, Token match)
        => new UnaryOperator(match.Value, false, Parse(input[..^match.Length]));
}
