using CalcExpr.Attributes;
using CalcExpr.Expressions;
using CalcExpr.Extensions;
using CalcExpr.Parsing.Rules;
using CalcExpr.Tokenization;
using CalcExpr.Tokenization.Tokens;
using System.Collections.Immutable;
using System.Reflection;
using System.Text.RegularExpressions;
using static CalcExpr.Parsing.Defaults.MatchFunctions;
using static CalcExpr.Parsing.Defaults.ParseFunctions;
using static CalcExpr.Parsing.Defaults.ParseMatchFunctions;

namespace CalcExpr.Parsing;

/// <summary>
/// Create a <see cref="Parser"/> using the specified grammar.
/// </summary>
/// <param name="grammar">
/// The specified <see cref="IEnumerable{Rule}"/> to be used as the grammar of the <see cref="Parser"/>.
/// </param>
public partial class Parser(IEnumerable<IParserRule> grammar)
{
    private readonly List<IParserRule> _grammar = grammar.ToList();
    // TODO: Change tokenizer initialization.
    private readonly Tokenizer _tokenizer = new();
    
    public IParserRule[] Grammar => [.. _grammar];

    // lang=regex
    private const string PREFIX = @"((\+{2})|(\-{2})|[\+\-!~¬])";
    // lang=regex
    private const string POSTFIX = @"((\+{2})|(\-{2})|((?<![^!](!!)*!)!!)|[!%#])";
    private const string OPERAND = @$"({PREFIX}*(\d|\w|[\[\{{\]\}}\u001A]){POSTFIX}*)";
    // lang=regex
    private const string ATTRIBUTE = @"(\w(\(\d(,\d)*\))?)";
    private const string PARAMETER = @$"((\[{ATTRIBUTE}(,{ATTRIBUTE})*\])?\w)";

    /// <summary>
    /// Creates a <see cref="Parser"/> with the default grammar.
    /// </summary>
    public Parser()
        : this([
            new ParserRule("Collection", ParseCollection, MatchCollection, ParseMatchCollection),
            new ParserRule("FunctionCall", ParseFunctionCall, MatchFunctionCall, ParseMatchFunctionCall),
            new RegexRule("LambdaFunction", @$"^({PARAMETER}|(\({PARAMETER}?\))|(\({PARAMETER}(,{PARAMETER})*\)))=>", ParseMatchLambdaFunction),
            new ParserRule("Parentheses", ParseMatchParentheses, MatchParentheses),
            new RegexRule("WithParentheses", @"[\(\)]", ParseMatchWithParentheses),
            new RegexRule("AssignBinOp", @$"(?<={OPERAND})(?<!!)(=)(?={OPERAND})", ParseMatchAssignmentOperator, RegexOptions.RightToLeft),
            new RegexRule("OrBinOp", @$"(?<={OPERAND})(\|\||∨)(?={OPERAND})", ParseMatchBinaryOperator, RegexOptions.RightToLeft),
            new RegexRule("XorBinOp", @$"(?<={OPERAND})(⊕)(?={OPERAND})", ParseMatchBinaryOperator, RegexOptions.RightToLeft),
            new RegexRule("AndBinOp", @$"(?<={OPERAND})(&&|∧)(?={OPERAND})", ParseMatchBinaryOperator, RegexOptions.RightToLeft),
            new RegexRule("EqBinOp", @$"(?<={OPERAND})(==|!=|<>|≠)(?={OPERAND})", ParseMatchBinaryOperator, RegexOptions.RightToLeft),
            new RegexRule("IneqBinOp", @$"(?<={OPERAND})(>=|<=|<(?!>)|(?<!<)>|[≤≥])(?={OPERAND})", ParseMatchBinaryOperator, RegexOptions.RightToLeft),
            new RegexRule("AddBinOp", @$"(?<={OPERAND})([\+\-])(?={OPERAND})", ParseMatchBinaryOperator, RegexOptions.RightToLeft),
            new RegexRule("MultBinOp", @$"(?<={OPERAND})(%%|//|[*×/÷%])(?={OPERAND})", ParseMatchBinaryOperator, RegexOptions.RightToLeft),
            new RegexRule("ExpBinOp", @"(?<=.)(\^)(?=.)", ParseMatchBinaryOperator, RegexOptions.RightToLeft),
            new RegexRule("Prefix", $"^{PREFIX}", ParseMatchPrefix),
            new RegexRule("Postfix", $"{POSTFIX}$", ParseMatchPostfix),
            new ParserRule("Indexer", ParseMatchIndexer, MatchIndexer),
            new OptionRule("Undefined", ["undefined", "dne"], ParseMatchUndefined),
            new OptionRule("Logical", ["true", "false"], ParseMatchLogical),
            new OptionRule("Infinity", ["∞", "inf", "infinity"], ParseMatchInfinity),
            new OptionRule("Constant", ["π", "pi", "τ", "tau", "empty_set", "empty", "∅", "e"], ParseMatchConstant),
            new TypeRule<WordToken>("Variable", ParseMatchVariable),
            new TypeRule<NumberToken>("Number", ParseMatchNumber),
        ])
    { }

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

        ImmutableArray<IToken> tokenizedInput = _tokenizer.Tokenize(input);
        IExpression result = Parse(tokenizedInput);

        AddCache(input, result);
        return result;
    }

    /// <summary>
    /// Parses a tokenized expression into an <see cref="IExpression"/>.
    /// </summary>
    /// <param name="input">The tokenized expression to parse.</param>
    /// <returns>An <see cref="IExpression"/> parsed from the specified expression <see cref="string"/>.</returns>
    /// <exception cref="Exception"></exception>
    public IExpression Parse(ImmutableArray<IToken> input)
    {
        if (ContainsCache(input))
            return _cache[input];

        foreach (IParserRule rule in _grammar)
        {
            if (rule.GetType().GetCustomAttribute<ReferenceRuleAttribute>() is not null)
                continue;

            IExpression? expression = rule.Parse(input, this);

            if (expression is not null)
                return expression;
        }

        throw new Exception($"The input was not in the correct format: '{input.JoinTokens()}'");
    }

    /// <summary>
    /// Add <see cref="IParserRule"/> to the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="rule">The <see cref="IParserRule"/> to be added to the grammar.</param>
    /// <param name="index">The index to put the <see cref="IParserRule"/> in the grammar.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="IParserRule"/> was successfully added to the grammar; otherwise, 
    /// <see langword="false"/>.
    /// </returns>
    public bool AddGrammarRule(IParserRule rule, int index = -1)
    {
        if (index < 0)
            index += _grammar.Count + 1;

        try
        {
            if (GrammarContains(rule.Name))
                return false;

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
    /// Removes the <see cref="IParserRule"/> with the specified name from the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="name">The name of the <see cref="IParserRule"/> to be removed.</param>
    /// <returns>
    /// The index of the removed rule if the <see cref="IParserRule"/> was successfully removed; otherwise, -1.
    /// </returns>
    public int RemoveGrammarRule(string name)
    {
        for (int i = 0; i < _grammar.Count; i++)
        {
            if (_grammar[i].Name == name)
            {
                if (RemoveGrammarRuleAt(i))
                    return i;
                else
                    break;
            }
        }

        return -1;
    }

    /// <summary>
    /// Removes the <see cref="IParserRule"/> at the specified index from the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="index">The index for the <see cref="IParserRule"/> to be removed.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="IParserRule"/> was successfully removed; otherwise, <see langword="false"/>.
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
    /// Replaces the current <see cref="IParserRule"/> with the specified name with a new <see cref="IParserRule"/>.
    /// </summary>
    /// <param name="newRule">The new rule to replace the old rule.</param>
    /// <returns>
    /// The index of the replaced rule if the <see cref="IParserRule"/> was successfully replaced; otherwise, -1.
    /// </returns>
    public int ReplaceGrammarRule(IParserRule newRule)
    {
        for (int i = 0; i < _grammar.Count; i++)
        {
            if (_grammar[i].Name == newRule.Name)
            {
                _grammar[i] = newRule;
                return i;
            }
        }

        return -1;
    }

    /// <summary>
    /// Determines whether a rule with the specified name is in the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="name">The name of the <see cref="IParserRule"/> to find.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="IParserRule"/> was successfully found; otherwise, <see langword="false"/>.
    /// </returns>
    public bool GrammarContains(string name)
    {
        foreach (IParserRule rule in _grammar)
            if (rule.Name == name)
                return true;

        return false;
    }

    /// <summary>
    /// Gets a <see cref="IParserRule"/> from the grammar based on the specified name.
    /// </summary>
    /// <param name="name">The name of the grammar rule.</param>
    /// <returns>The <see cref="IParserRule"/> in the grammar with the name <paramref name="name"/>.</returns>
    public IParserRule? GetGrammarRule(string name)
    {
        int idx = 0;

        while (idx < _grammar.Count && _grammar[idx].Name != name)
            idx++;

        return GetGrammarRule(idx);
    }

    /// <summary>
    /// Gets a <see cref="IParserRule"/> from the grammar based on the specified index.
    /// </summary>
    /// <param name="index">The index of the grammar rule.</param>
    /// <returns>The <see cref="IParserRule"/> in the grammar at the index of <paramref name="index"/>.</returns>
    public IParserRule? GetGrammarRule(int index)
        => index >= 0 && index < _grammar.Count ? _grammar[index] : null;
}
