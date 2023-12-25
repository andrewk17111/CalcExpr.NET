using CalcExpr.Attributes;
using CalcExpr.Exceptions;
using CalcExpr.Expressions;
using CalcExpr.Parsing.Rules;
using System.Reflection;
using System.Text.RegularExpressions;

namespace CalcExpr.Parsing;

public class Parser
{
    private readonly List<Rule> _grammar = [];
    private readonly Dictionary<string, IExpression> _cache = [];

    public Rule[] Grammar
        => _grammar.ToArray();

    public string[] Cache
        => _cache.Keys.ToArray();

    /// <summary>
    /// Creates a <see cref="Parser"/> with the default grammar.
    /// </summary>
    public Parser()
    {
        _grammar =
        [
            new ReferenceRegexRule("Operand", "({Prefix}*({Variable}|{Constant}|{Number}|{Token}){Postfix}*)"),
            new ReferenceRegexRule("Token", @"\[\d+\]"),
            new Rule("Parentheses", ParseParentheses, MatchParentheses),
            new RegexRule("WithParentheses", @"\(|\)", RegexOptions.None, ParseWithParentheses),
            new NestedRegexRule("AssignBinOp", @"(?<={Operand})(?<!!)(=)(?={Operand})",
                RegexRuleOptions.RightToLeft | RegexRuleOptions.PadReferences, ParseBinaryOperator),
            new NestedRegexRule("OrBinOp", @"(?<={Operand})(\|\||∨)(?={Operand})",
                RegexRuleOptions.RightToLeft | RegexRuleOptions.PadReferences, ParseBinaryOperator),
            new NestedRegexRule("XorBinOp", @"(?<={Operand})(⊕)(?={Operand})",
                RegexRuleOptions.RightToLeft | RegexRuleOptions.PadReferences, ParseBinaryOperator),
            new NestedRegexRule("AndBinOp", @"(?<={Operand})(&&|∧)(?={Operand})",
                RegexRuleOptions.RightToLeft | RegexRuleOptions.PadReferences, ParseBinaryOperator),
            new NestedRegexRule("EqBinOp", @"(?<={Operand})(==|!=|<>|≠)(?={Operand})",
                RegexRuleOptions.RightToLeft | RegexRuleOptions.PadReferences, ParseBinaryOperator),
            new NestedRegexRule("IneqBinOp", @"(?<={Operand})(>=|<=|<(?!>)|(?<!<)>|[≤≥])(?={Operand})",
                RegexRuleOptions.RightToLeft | RegexRuleOptions.PadReferences, ParseBinaryOperator),
            new NestedRegexRule("AddBinOp", @"(?<={Operand})([\+\-])(?={Operand})",
                RegexRuleOptions.RightToLeft | RegexRuleOptions.PadReferences, ParseBinaryOperator),
            new NestedRegexRule("MultBinOp", @"(?<={Operand})(%%|//|[*×/÷%])(?={Operand})",
                RegexRuleOptions.RightToLeft | RegexRuleOptions.PadReferences, ParseBinaryOperator),
            new NestedRegexRule("ExpBinOp", @"(?<={Operand})(\^)(?={Operand})",
                RegexRuleOptions.RightToLeft | RegexRuleOptions.PadReferences, ParseBinaryOperator),
            new RegexRule("Prefix", @"((\+{2})|(\-{2})|[\+\-!~¬])",
                RegexRuleOptions.Left | RegexRuleOptions.TrimLeft, ParsePrefix),
            new RegexRule("Postfix", @"((\+{2})|(\-{2})|((?<![A-Za-zΑ-Ωα-ω0-9](!!)*!)!!)|[!%#])",
                RegexRuleOptions.RightToLeft | RegexRuleOptions.Right | RegexRuleOptions.TrimRight, ParsePostfix),
            new RegexRule("Constant", "(∞|(inf(inity)?)|π|pi|τ|tau|e|true|false)",
                RegexRuleOptions.Only | RegexRuleOptions.Trim, ParseConstant),
            new RegexRule("Variable", "([A-Za-zΑ-Ωα-ω]+(_[A-Za-zΑ-Ωα-ω0-9]+)*)",
                RegexRuleOptions.Only | RegexRuleOptions.Trim, ParseVariable),
            new RegexRule("Number", @"((\d+\.?\d*)|(\d*\.?\d+))", RegexRuleOptions.Only | RegexRuleOptions.Trim,
                ParseNumber)
        ];
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
        ArgumentNullException.ThrowIfNull(input);

        if (ContainsCache(input))
            return _cache[CleanExpressionString(input)].Clone();

        foreach (Rule rule in _grammar)
        {
            if (rule.GetType().GetCustomAttribute<ReferenceRuleAttribute>() is not null)
                continue;

            Token? match = rule.Match(input, Grammar);

            if (match.HasValue)
            {
                IExpression expression = rule.Parse.Invoke(input, match.Value, this);

                AddCache(input, expression);
                return expression;
            }
        }

        throw new Exception($"The input was not in the correct format: '{input}'");
    }

    private static string CleanExpressionString(string expression)
        => Regex.Replace(expression, @"\s+", "");

    private static string TokenizeInput(string input, out Token[] tokens)
    {
        input = Regex.Replace(input, @"[\[\]\\]", match => @$"\{match.Value}");

        List<Token> toks = [];
        string output = String.Empty;
        int start = -1;
        int depth = 0;

        for (int i = 0; i < input.Length; i++)
        {
            char current = input[i];

            if (current == '(')
            {
                if (start == -1)
                {
                    start = i;
                    depth = 1;
                }
                else
                {
                    depth++;
                }
            }
            else if (current == ')')
            {
                if (start < 0)
                {
                    throw new UnbalancedParenthesesException(input);
                }
                else if (depth > 1)
                {
                    depth--;
                }
                else
                {
                    output += $"[{toks.Count}]";
                    toks.Add(new Token(input[start..(i + 1)], start));
                    start = -1;
                    depth = 0;
                }
            }
            else
            {
                if (start < 0)
                    output += current;
                else if (i == input.Length - 1)
                    throw new UnbalancedParenthesesException(input);
            }
        }

        tokens = [.. toks];
        return output;
    }

    private static int DetokenizeIndex(int index, string tokenized_string, Token[] tokens)
    {
        string[] matches =
            [.. Regex.Matches(tokenized_string[..index], @"((?<=^|([^\\]([\\][\\])*))\[\d+\])|(\\[\[\]\\])")
                .Select(m => m.Value)];

        foreach (string match in matches)
            if (match.StartsWith('\\'))
                index -= 1;
            else
                index += tokens[Convert.ToInt32(match[1..^1])].Length - 3;

        return index;
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
        => _cache.ContainsKey(CleanExpressionString(expression));

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
    {
        try
        {
            _cache[CleanExpressionString(key)] = value.Clone();
            return true;
        }
        catch
        {
            return false;
        }
    }

    /// <summary>
    /// Removes a cached <see cref="IExpression"/> based on the specified expression <see cref="string"/>.
    /// </summary>
    /// <param name="expression">The expression <see cref="string"/> of the cached <see cref="IExpression"/>.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="IExpression"/> was successfully removed from the cache; otherwise,
    /// <see langword="false"/>.
    /// </returns>
    public bool RemoveCache(string expression)
    {
        string clean_expression = CleanExpressionString(expression);

        return _cache.ContainsKey(clean_expression) && _cache.Remove(clean_expression);
    }

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
    /// Removes the <see cref="Rule"/> with the specified name from the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="name">The name of the <see cref="Rule"/> to be removed.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully removed; otherwise, <see langword="false"/>.
    /// </returns>
    public bool RemoveGrammarRule(string name)
    {
        for (int i = 0; i < _grammar.Count; i++)
            if (_grammar[i].Name == name)
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
    /// Determines whether a rule with the specified name is in the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="name">The name of the <see cref="Rule"/> to find.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully found; otherwise, <see langword="false"/>.
    /// </returns>
    public bool GrammarContains(string name)
    {
        foreach (Rule rule in _grammar)
            if (rule.Name == name)
                return true;

        return false;
    }

    private Token? MatchParentheses(string input, IEnumerable<Rule> rules)
    {
        input = input.Trim();

        if (input[0] == '(' && input[^1] == ')')
        {
            int depth = 0;

            for (int i = 1; i < input.Length - 1; i++)
            {
                char current = input[i];

                if (current == '(')
                {
                    depth++;
                }
                else if (current == ')')
                {
                    if (depth == 0)
                        return null;

                    depth--;
                }
            }

            if (depth == 0)
                return new Token(input[1..^1], 1);
        }

        return null;
    }

    private Parentheses ParseParentheses(string input, Token token, Parser parser)
        => new Parentheses(Parse(token));

    private IExpression ParseWithParentheses(string input, Token _, Parser parser)
    {
        string tokenized_input = TokenizeInput(input, out Token[] tokens);

        foreach (Rule rule in parser.Grammar)
        {
            if (rule.GetType().GetCustomAttribute<ReferenceRuleAttribute>() is not null)
                continue;

            Token? match = rule.Match(tokenized_input, Grammar);

            if (match.HasValue)
                return rule.Parse.Invoke(input,
                    new Token(match.Value, DetokenizeIndex(match.Value.Index, tokenized_input, tokens)),
                    this);
        }

        throw new Exception($"The input was not in the correct format: '{input}'");
    }

    private BinaryOperator ParseBinaryOperator(string input, Token match, Parser parser)
        => new BinaryOperator(match.Value, Parse(input[..match.Index]), Parse(input[(match.Index + match.Length)..]));

    private UnaryOperator ParsePrefix(string input, Token match, Parser parser)
        => new UnaryOperator(match.Value, true, Parse(input[(match.Index + match.Length)..]));

    private UnaryOperator ParsePostfix(string input, Token match, Parser parser)
        => new UnaryOperator(match.Value, false, Parse(input[..match.Index]));

    private Constant ParseConstant(string input, Token match, Parser parser)
        => new Constant(match.Value);

    private Variable ParseVariable(string input, Token match, Parser parser)
        => new Variable(match.Value);

    private static Number ParseNumber(string input, Token match, Parser parser)
        => new Number(Convert.ToDouble(match.Value));
}
