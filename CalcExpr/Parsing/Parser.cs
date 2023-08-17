using CalcExpr.Expressions;
using System.Data;
using System.Text.RegularExpressions;

namespace CalcExpr.Parsing;

public class Parser
{
    private readonly List<Rule> _grammar = new List<Rule>();

    public Rule[] Grammar
        => _grammar.ToArray();

    /// <summary>
    /// Creates a <see cref="Parser"/> with the default grammar.
    /// </summary>
    public Parser()
    {
        _grammar = new List<Rule>()
        {
            new Rule(@"(?<=^\s*)[\+\-!~¬]", ParsePrefix),
            new Rule(@"[%!](?=\s*$)", ParsePostfix),
            new Rule(@"(?<=^\s*)((\d+\.?\d*)|(\d*\.?\d+))(?=\s*$)", ParseNumber)
        };
    }

    /// <summary>
    /// Create a <see cref="Parser"/> using the specified grammar.
    /// </summary>
    /// <param name="grammar">
    /// The specified <see cref="IEnumerable{Rule}"/> to be used as the grammar of the <see cref="Parser"/>.
    /// </param>
    public Parser(IEnumerable<Rule> grammar)
        => _grammar = grammar.ToList();

    /// <summary>
    /// Parses an expression <see cref="string"/> into an <see cref="IExpression"/>.
    /// </summary>
    /// <param name="input">The expression <see cref="string"/> to parse.</param>
    /// <returns>An <see cref="IExpression"/> parsed from the specified expression <see cref="string"/>.</returns>
    /// <exception cref="ArgumentNullException"></exception>
    /// <exception cref="Exception"></exception>
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
    {
        if (index < 0)
            index += _grammar.Count + 1;

        try
        {
            if (index <= 0)
                _grammar.Insert(0, rule);
            else if (index >= _grammar.Count)
                _grammar.Insert(_grammar.Count, rule);
            else
                _grammar.Insert(index, rule);

            return true;
        }
        catch
        {
            return false;
        }
    }

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
    {
        for (int i = 0; i < _grammar.Count; i++)
            if (_grammar[i].RegularExpression == regex)
                return RemoveGrammarRuleAt(i);

        return false;
    }

    /// <summary>
    /// Removes the <see cref="Rule"/> at the specified index from the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="index">The index for the <see cref="Rule"/> to be removed.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully removed; otherwise, <see langword="false"/>.
    /// </returns>
    public bool RemoveGrammarRuleAt(int index)
    {
        try
        {
            _grammar.RemoveAt(index);
            return true;
        }
        catch
        {
            return false;
        }
    }

    /// <summary>
    /// Determines whether a rule with the specified regex <see cref="string"/> is in the grammar of the 
    /// <see cref="Parser"/>.
    /// </summary>
    /// <param name="regex">The regex <see cref="string"/> of the <see cref="Rule"/> to find.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully found; otherwise, <see langword="false"/>.
    /// </returns>
    public bool GrammarContains(string regex)
    {
        foreach (Rule rule in _grammar)
            if (rule.RegularExpression == regex)
                return true;

        return false;
    }

    private static IExpression ParseNumber(string input, Token match)
        => new Number(Convert.ToDouble(match.Value));

    private IExpression ParsePrefix(string input, Token match)
        => new UnaryOperator(match.Value, true, Parse(input[match.Length..]));

    private IExpression ParsePostfix(string input, Token match)
        => new UnaryOperator(match.Value, false, Parse(input[..^match.Length]));
}
