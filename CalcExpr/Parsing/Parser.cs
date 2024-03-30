using CalcExpr.Attributes;
using CalcExpr.Exceptions;
using CalcExpr.Expressions;
using CalcExpr.Expressions.Collections;
using CalcExpr.Expressions.Components;
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
    /// <param name="build_rules">Whether or not grammar rules should be prebuilt.</param>
    public Parser(bool build_rules = true)
        : this ([
            new ReferenceRegexRule("DiscreteOperand", "({Prefix}*({Variable}|{Constant}|{Number}|{Token}){Postfix}*)",
                RegexRuleOptions.PadReferences),
            new ReferenceRegexRule("Operand", @"[\[\{]?({DiscreteOperand}|{Parameter}|{TokenizedParameter})[\]\}]?"),
            new ReferenceRegexRule("Token", @"\[\d+\]"),
            new ReferenceRegexRule("Attribute", @"([A-Za-z][A-Za-z_0-9]*(\({Number}(,{Number})*\))?)",
                RegexRuleOptions.PadReferences),
            new ReferenceRegexRule("Parameter", @"((\\?\[{Attribute}(,{Attribute})*\\?\])?{Variable})",
                RegexRuleOptions.PadReferences),
            new ReferenceRegexRule("TokenizedAttribute", @"([A-Za-z][A-Za-z_0-9]*({Token})?)",
                RegexRuleOptions.PadReferences),
            new ReferenceRegexRule("TokenizedParameter",
                @"((\\?\[{TokenizedAttribute}(,{TokenizedAttribute})*\\?\])?{Variable})",
                RegexRuleOptions.PadReferences),
            new Rule("Collection", ParseCollection, MatchCollection),
            new Rule("FunctionCall", ParseFunctionCall, MatchFunctionCall),
            new NestedRegexRule("LambdaFunction", @"({Parameter}|\(\s*(({Parameter},)*{Parameter})?\))\s*=>",
                RegexRuleOptions.Left | RegexRuleOptions.PadReferences | RegexRuleOptions.Trim, ParseLambdaFunction),
            new Rule("Parentheses", ParseParentheses, MatchParentheses),
            new RegexRule("WithParentheses", @"\(|\)", RegexOptions.None, ParseWithParentheses),
            new NestedRegexRule("AssignBinOp", @"(?<={Operand})(?<!!)(=)(?={Operand})",
                RegexRuleOptions.RightToLeft | RegexRuleOptions.PadReferences, ParseAssignmentOperator),
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
            new RegexRule("Constant", "(∞|(inf(inity)?)|π|pi|τ|tau|e|true|false|undefined|dne)",
                RegexRuleOptions.Only | RegexRuleOptions.Trim, ParseConstant),
            new RegexRule("Variable", "([A-Za-zΑ-Ωα-ω]+(_[A-Za-zΑ-Ωα-ω0-9]+)*)",
                RegexRuleOptions.Only | RegexRuleOptions.Trim, ParseVariable),
            new RegexRule("Number", @"((\d+\.?\d*)|(\d*\.?\d+))", RegexRuleOptions.Only | RegexRuleOptions.Trim,
                ParseNumber)
        ],
        build_rules)
    { }

    /// <summary>
    /// Create a <see cref="Parser"/> using the specified grammar.
    /// </summary>
    /// <param name="grammar">
    /// The specified <see cref="IEnumerable{Rule}"/> to be used as the grammar of the <see cref="Parser"/>.
    /// </param>
    /// <param name="build_rules">Whether or not grammar rules should be prebuilt.</param>
    public Parser(IEnumerable<Rule> grammar, bool build_rules = true)
    {
        _grammar = grammar.ToList();

        if (build_rules)
            BuildGrammarRules(_grammar);
    }

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
            return _cache[CleanExpressionString(input)];

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
            _cache[CleanExpressionString(key)] = value;
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
    /// Clears the cache of the parser.
    /// </summary>
    /// <returns>
    /// <see langword="true"/> if the cache was successfully cleared; otherwise <see langword="false"/>.
    /// </returns>
    public bool ClearCache()
    {
        _cache.Clear();
        return _cache.Count == 0;
    }

    /// <summary>
    /// Add <see cref="Rule"/> to the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="rule">The <see cref="Rule"/> to be added to the grammar.</param>
    /// <param name="index">The index to put the <see cref="Rule"/> in the grammar.</param>
    /// <param name="build_rules">Whether or not grammar rules should be rebuilt.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully added to the grammar; otherwise, 
    /// <see langword="false"/>.
    /// </returns>
    public bool AddGrammarRule(Rule rule, int index = -1, bool build_rules = true)
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

            if (build_rules)
                RebuildGrammarRules();

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
    /// <param name="build_rules">Whether or not grammar rules should be rebuilt.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully removed; otherwise, <see langword="false"/>.
    /// </returns>
    public bool RemoveGrammarRule(string name, bool build_rules = true)
    {
        for (int i = 0; i < _grammar.Count; i++)
            if (_grammar[i].Name == name)
                return RemoveGrammarRuleAt(i, build_rules);

        return false;
    }

    /// <summary>
    /// Removes the <see cref="Rule"/> at the specified index from the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="index">The index for the <see cref="Rule"/> to be removed.</param>
    /// <param name="build_rules">Whether or not grammar rules should be rebuilt.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully removed; otherwise, <see langword="false"/>.
    /// </returns>
    public bool RemoveGrammarRuleAt(int index, bool build_rules = true)
    {
        try
        {
            _grammar.RemoveAt(index);

            if (build_rules)
                RebuildGrammarRules();

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

    /// <summary>
    /// Gets a <see cref="Rule"/> from the grammar based on the specified name.
    /// </summary>
    /// <param name="name">The name of the grammar rule.</param>
    /// <returns>The <see cref="Rule"/> in the grammar with the name <paramref name="name"/>.</returns>
    public Rule? GetGrammarRule(string name)
    {
        int idx = 0;

        while (idx < _grammar.Count && _grammar[idx].Name != name)
            idx++;

        return GetGrammarRule(idx);
    }

    /// <summary>
    /// Gets a <see cref="Rule"/> from the grammar based on the specified index.
    /// </summary>
    /// <param name="index">The index of the grammar rule.</param>
    /// <returns>The <see cref="Rule"/> in the grammar at the index of <paramref name="index"/>.</returns>
    public Rule? GetGrammarRule(int index)
        => index >= 0 && index < _grammar.Count ? _grammar[index] : null;

    /// <summary>
    /// Rebuilds all of the grammar rules in the <see cref="Parser"/>.
    /// </summary>
    public void RebuildGrammarRules()
        => BuildGrammarRules(_grammar);

    private static void BuildGrammarRules(IEnumerable<Rule> rules)
    {
        foreach (Rule rule in rules)
            if (rule is NestedRegexRule regex_rule)
                regex_rule.Build(rules);
    }

    private static Token? MatchParentheses(string input, IEnumerable<Rule> rules)
    {
        input = input.Trim();

        if (String.IsNullOrWhiteSpace(input))
            return null;

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

    private static Token? MatchCollection(string input, IEnumerable<Rule> rules)
    {
        Match match = Regex.Match(input, @"(?<=^\s*)[\[\{].*?[\]\}](?=\s*$)");

        if (match.Success && match.Value[^1] - match.Value[0] == 2)
        {
            try
            {
                if (ContextFreeUtils.CheckBalancedBrackets(match.Value[1..^1], Brackets.Square | Brackets.Curly))
                    return match;
            }
            catch
            {
                throw;
            }
        }

        return null;
    }

    private static Token? MatchFunctionCall(string input, IEnumerable<Rule> rules)
    {
        input = input.Trim();

        if (String.IsNullOrWhiteSpace(input))
            return null;

        Match function_name = Regex.Match(input, @"^([A-Za-zΑ-Ωα-ω]+(_[A-Za-zΑ-Ωα-ω0-9]+)*)");

        if (function_name.Success)
        {
            Token? parentheses = MatchParentheses(input[function_name.Length..], rules);

            if (parentheses is not null)
                return new Token(
                    input[..(function_name.Length + parentheses.Value.Index + parentheses.Value.Length + 1)],
                    0);
        }

        return null;
    }

    private static IEnumerableExpression ParseCollection(string input, Token token, Parser parser)
    {
        string tokenized = ContextFreeUtils.TokenizeInput(token.Value[1..^1], out Token[] tokens,
            Brackets.Square | Brackets.Curly);
        IEnumerable<IExpression> enumerable = tokenized.Split(',')
            .Select(e => parser.Parse(Regex.Replace(e, @"(?<!(^|[^\\])\\(\\\\)*)\[\d+\]", m => m.Value[1..^1])));

        return token.Value[0] == '['
            ? new Vector(enumerable)
            : new Set(enumerable);
    }

    private static FunctionCall ParseFunctionCall(string input, Token token, Parser parser)
    {
        Match function_name = Regex.Match(input, @"(?<=^\s*)([A-Za-zΑ-Ωα-ω]+(_[A-Za-zΑ-Ωα-ω0-9]+)*)");
        string tokenized_args = token.Value[(function_name.Length + 1)..^1].TokenizeInput(out Token[] tokens);

        string[] args = tokenized_args
            .Split(",", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
            .Select(arg => !arg.Contains('[')
                ? arg
                : Regex.Replace(arg, @"\[\d+\]", match => tokens[Convert.ToInt32(match.Value[1..^1])]))
            .ToArray();

        return new FunctionCall(function_name.Value, args.Select(arg => parser.Parse(arg)));
    }

    private static LambdaFunction ParseLambdaFunction(string input, Token token, Parser parser)
    {
        string parameters_string = Regex.Match(token.Value.Trim(), @"(?<=^\(?).*?(?=\)?\s*=>)").Value.TrimStart('(');
        string tokenized_parameters_string = parameters_string.TokenizeInput(out Token[] attribute_tokens,
            Brackets.Square);
        IEnumerable<Parameter> parameters = tokenized_parameters_string
            .Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
            .Select(p =>
            {
                Match attribute_match = Regex.Match(p, @"(?<=^\s*\[)\d+(?=\])");
                string? attribute_string = attribute_match.Success
                    ? attribute_tokens[Convert.ToInt32(attribute_match.Value)].Value[1..^1]
                    : null;
                IEnumerable<string> attributes = attribute_string is null
                    ? Enumerable.Empty<string>()
                    : Regex.Split(attribute_string, @"(?<!\([^\)]*?),(?![^\(]*?\))");

                return new Parameter(Regex.Match(p,
                    @$"(?<=\]?\s*){((RegexRule?)parser.GetGrammarRule("Variable"))!.RegularExpression}(?=\s*$)").Value,
                    attributes);
            }).ToArray();

        return new LambdaFunction(parameters, parser.Parse(input[(token.Index + token.Length)..]));
    }

    private static Parentheses ParseParentheses(string input, Token token, Parser parser)
        => new Parentheses(parser.Parse(token));

    private static IExpression ParseWithParentheses(string input, Token _, Parser parser)
    {
        string tokenized_input = input.TokenizeInput(out Token[] tokens, Brackets.Parenthesis);

        foreach (Rule rule in parser.Grammar)
        {
            if (rule.GetType().GetCustomAttribute<ReferenceRuleAttribute>() is not null)
                continue;

            Token? match = rule.Match(tokenized_input, parser.Grammar);

            if (match.HasValue)
                return rule.Parse.Invoke(input,
                    new Token(match.Value, ContextFreeUtils.DetokenizeIndex(match.Value.Index, tokenized_input,
                        tokens)),
                    parser);
        }

        throw new Exception($"The input was not in the correct format: '{input}'");
    }

    private static AssignmentOperator ParseAssignmentOperator(string input, Token match, Parser parser)
        => new AssignmentOperator((parser.Parse(input[..match.Index]) as Variable)!,
            parser.Parse(input[(match.Index + match.Length)..]));

    private static BinaryOperator ParseBinaryOperator(string input, Token match, Parser parser)
        => new BinaryOperator(match.Value, parser.Parse(input[..match.Index]),
            parser.Parse(input[(match.Index + match.Length)..]));

    private static UnaryOperator ParsePrefix(string input, Token match, Parser parser)
        => new UnaryOperator(match.Value, true, parser.Parse(input[(match.Index + match.Length)..]));

    private static UnaryOperator ParsePostfix(string input, Token match, Parser parser)
        => new UnaryOperator(match.Value, false, parser.Parse(input[..match.Index]));

    private static Constant ParseConstant(string input, Token match, Parser parser)
        => new Constant(match.Value);

    private static Variable ParseVariable(string input, Token match, Parser parser)
        => new Variable(match.Value);

    private static Number ParseNumber(string input, Token match, Parser parser)
        => new Number(Convert.ToDouble(match.Value));
}
