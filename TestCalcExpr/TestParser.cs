using CalcExpr.Expressions;
using CalcExpr.Parsing;
using CalcExpr.Parsing.Rules;
using System.Text.RegularExpressions;

namespace TestCalcExpr;

[TestClass]
public class TestParser
{
    readonly RegexRule CUSTOM_RULE = new RegexRule("Char", @"[A-Z]", RegexOptions.None,
        (expression, token, _) => new Number(token.Value[0] - 65));
    readonly Dictionary<string, IExpression> EXPRESSIONS = new Dictionary<string, IExpression>()
    {
        { ".1", new Number(0.1) },
        { "0.1", new Number(0.1) },
        { "1.", new Number(1) },
        { "1", new Number(1) },
        { "+1", new UnaryOperator("+", true, new Number(1)) },
        { "-1", new UnaryOperator("-", true, new Number(1)) },
        { "!1", new UnaryOperator("!", true, new Number(1)) },
        { "~2", new UnaryOperator("~", true, new Number(2)) },
        { "¬0", new UnaryOperator("¬", true, new Number(0)) },
        { "5!", new UnaryOperator("!", false, new Number(5)) },
        { "5!!", new UnaryOperator("!!", false, new Number(5)) },
        { "1%", new UnaryOperator("%", false, new Number(1)) },
        { "~!1", new UnaryOperator("~", true, new UnaryOperator("!", true, new Number(1))) },
        { "2!%", new UnaryOperator("%", false, new UnaryOperator("!", false, new Number(2))) },
        { "-5%", new UnaryOperator("-", true, new UnaryOperator("%", false, new Number(5))) },
        { "3!!!", new UnaryOperator("!", false, new UnaryOperator("!!", false, new Number(3))) },
        { "1+2.0", new BinaryOperator("+", new Number(1), new Number(2)) },
        { "0 + 0 * 2", new BinaryOperator("+", new Number(0), new BinaryOperator("*", new Number(0), new Number(2))) },
        { "1.0-2", new BinaryOperator("-", new Number(1), new Number(2)) },
        { "2*3", new BinaryOperator("*", new Number(2), new Number(3)) },
        { "6×7", new BinaryOperator("×", new Number(6), new Number(7)) },
        { "1/2", new BinaryOperator("/", new Number(1), new Number(2)) },
        { "2÷2", new BinaryOperator("÷", new Number(2), new Number(2)) },
        { "2^3", new BinaryOperator("^", new Number(2), new Number(3)) },
        { "13%12", new BinaryOperator("%", new Number(13), new Number(12)) },
        { "13.2%%12.5", new BinaryOperator("%%", new Number(13.2), new Number(12.5)) },
        { "13//12", new BinaryOperator("//", new Number(13), new Number(12)) },
        { "1&&2", new BinaryOperator("&&", new Number(1), new Number(2)) },
        { "0∧0", new BinaryOperator("∧", new Number(0), new Number(0)) },
        { "1||2", new BinaryOperator("||", new Number(1), new Number(2)) },
        { "0∨0", new BinaryOperator("∨", new Number(0), new Number(0)) },
        { "1⊕1", new BinaryOperator("⊕", new Number(1), new Number(1)) },
        { "1==2", new BinaryOperator("==", new Number(1), new Number(2)) },
        { "1!=2", new BinaryOperator("!=", new Number(1), new Number(2)) },
        { "1≠1", new BinaryOperator("≠", new Number(1), new Number(1)) },
        { "1<>2", new BinaryOperator("<>", new Number(1), new Number(2)) },
        { "1<2", new BinaryOperator("<", new Number(1), new Number(2)) },
        { "1>2", new BinaryOperator(">", new Number(1), new Number(2)) },
        { "1>=2", new BinaryOperator(">=", new Number(1), new Number(2)) },
        { "2≥2", new BinaryOperator("≥", new Number(2), new Number(2)) },
        { "1<=2", new BinaryOperator("<=", new Number(1), new Number(2)) },
        { "1≤1", new BinaryOperator("≤", new Number(1), new Number(1)) },
        { "1+2-3*4/5^7&&8||9⊕10==11", new BinaryOperator("||", new BinaryOperator("&&", new BinaryOperator("-",
            new BinaryOperator("+", new Number(1), new Number(2)), new BinaryOperator("/", new BinaryOperator("*",
                new Number(3), new Number(4)), new BinaryOperator("^", new Number(5), new Number(7)))), new Number(8)),
            new BinaryOperator("⊕", new Number(9), new BinaryOperator("==", new Number(10), new Number(11)))) },
        { "+1+-2.0", new BinaryOperator("+", new UnaryOperator("+", true, new Number(1)), new UnaryOperator("-", true,
            new Number(2))) },
        { "1.0!-2%", new BinaryOperator("-", new UnaryOperator("!", false, new Number(1)), new UnaryOperator("%", false,
            new Number(2))) },
        { "!¬2*-+3", new BinaryOperator("*", new UnaryOperator("!", true, new UnaryOperator("¬", true, new Number(2))),
            new UnaryOperator("-", true, new UnaryOperator("+", true, new Number(3)))) },
        { "1%%/2!!", new BinaryOperator("/", new UnaryOperator("%", false, new UnaryOperator("%", false,
            new Number(1))), new UnaryOperator("!!", false, new Number(2))) },
        { "-13!%!12%", new BinaryOperator("%", new UnaryOperator("-", true, new UnaryOperator("!", false,
            new Number(13))), new UnaryOperator("!", true, new UnaryOperator("%", false, new Number(12)))) },
        { "1+((2-τ)*(4/-pi))^7&&abc_1||9⊕10==11", new BinaryOperator("||", new BinaryOperator("&&", new BinaryOperator("+",
            new Number(1), new BinaryOperator("^", new Parentheses(new BinaryOperator("*", new Parentheses(
                new BinaryOperator("-", new Number(2), new Constant("τ"))), new Parentheses( new BinaryOperator("/",
                    new Number(4), new UnaryOperator("-", true, new Constant("pi")))))), new Number(7))),
            new Variable("abc_1")), new BinaryOperator("⊕", new Number(9), new BinaryOperator("==", new Number(10),
                new Number(11)))) },
        { "∞", new Constant("∞") },
        { "inf", new Constant("inf") },
        { "infinity", new Constant("infinity") },
        { "π", new Constant("π") },
        { "pi", new Constant("pi") },
        { "τ", new Constant("τ") },
        { "tau", new Constant("tau") },
        { "e", new Constant("e") },
        { "true", new Constant("true") },
        { "false", new Constant("false") },
        { "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz",
            new Variable("AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz") },
        { "ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛλΜμΝνΞξΟοΠπΡρΣσςΤτΥυΦφΧχΨψΩω",
            new Variable("ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛλΜμΝνΞξΟοΠπΡρΣσςΤτΥυΦφΧχΨψΩω") },
        { "abc_123", new Variable("abc_123") },
        { "αβγ_123", new Variable("αβγ_123") },
        { "abcd_αβγ_xyz", new Variable("abcd_αβγ_xyz") },
    };

    /// <summary>
    /// Tests that the Parser can be initialized properly from either constructor.
    /// </summary>
    [TestMethod]
    public void TestInit()
    {
        const string OPERAND = @"({Prefix}*({Variable}|{Constant}|{Number}|\[\d+\]){Postfix}*)";

        (string, string)[] default_rules =
        {
            ("OrBinOp", @$"(?<={OPERAND})(\|\||∨)(?={OPERAND})"),
            ("XorBinOp", @$"(?<={OPERAND})(⊕)(?={OPERAND})"),
            ("AndBinOp", @$"(?<={OPERAND})(&&|∧)(?={OPERAND})"),
            ("EqBinOp", @$"(?<={OPERAND})(==|!=|<>|≠)(?={OPERAND})"),
            ("IneqBinOp", @$"(?<={OPERAND})(>=|<=|<(?!>)|(?<!<)>|[≤≥])(?={OPERAND})"),
            ("AddBinOp", @$"(?<={OPERAND})([\+\-])(?={OPERAND})"),
            ("MultBinOp", @$"(?<={OPERAND})(%%|//|[*×/÷%])(?={OPERAND})"),
            ("ExpBinOp", @$"(?<={OPERAND})(\^)(?={OPERAND})"),
            ("Prefix", @"[\+\-!~¬]"),
            ("Postfix", @"(((?<![A-Za-zΑ-Ωα-ω0-9](!!)*!)!!)|[!%])"),
            ("Constant", "(∞|(inf(inity)?)|π|pi|τ|tau|e|true|false)"),
            ("Variable", "([A-Za-zΑ-Ωα-ω]+(_[A-Za-zΑ-Ωα-ω0-9]+)*)"),
            ("Number", @"((\d+\.?\d*)|(\d*\.?\d+))"),
        };

        Parser parser = new Parser();

        for (int i = 0; i < default_rules.Length; i++)
        {
            RegexRule rr = (RegexRule)parser.Grammar[i];

            Assert.AreEqual(rr.Name, default_rules[i].Item1);
            Assert.AreEqual(rr.RegularExpression, default_rules[i].Item2);
        }

        parser = new Parser(new Rule[] { CUSTOM_RULE });

        Assert.IsTrue(parser.Grammar.Length == 1);
        Assert.IsTrue(parser.Grammar[0] == CUSTOM_RULE);
    }

    /// <summary>
    /// Tests that the Parser is able to properly parse strings into IExpressions.
    /// </summary>
    [TestMethod]
    public void TestParse()
    {
        foreach (string expression in EXPRESSIONS.Keys)
            Assert.AreEqual(EXPRESSIONS[expression], new Parser().Parse(expression));
    }

    /// <summary>
    /// Tests that previously parsed expressions get added to the Parser's cache, expressions can be manually added and
    /// removed from the cache, a string can be found in the cache, and the cache can be cleared.
    /// </summary>
    [TestMethod]
    public void TestCache()
    {
        Parser parser = new Parser();

        foreach (string expression in EXPRESSIONS.Keys)
        {
            parser.Parse(expression);

            bool cached = parser.ContainsCache(expression);

            if (Regex.IsMatch(expression, @"\(|\)"))
            {
                Assert.IsFalse(cached);
            }
            else
            {
                Assert.IsTrue(cached);
                parser.RemoveCache(expression);
                Assert.IsFalse(parser.ContainsCache(expression));
            }
        }

        (string, IExpression) pi = ("pi", new Number(3.1415926535));
        
        parser.AddCache(pi.Item1, pi.Item2);
        Assert.IsTrue(parser.ContainsCache(pi.Item1));
        parser.RemoveCache(pi.Item1);
        Assert.IsFalse(parser.ContainsCache(pi.Item1));
    }

    /// <summary>
    /// Tests that Rules can be added and removed from the Parser's grammar along with being able to change the priority
    /// of Rules.
    /// </summary>
    [TestMethod]
    public void TestGrammar()
    {
        Parser parser = new Parser();
        RegexRule tau = new RegexRule("tau", "tau", RegexOptions.None, (expression, token, _) => new Number(6.28));
        
        Assert.IsFalse(parser.Grammar.Contains(CUSTOM_RULE));
        Assert.IsFalse(parser.Grammar.Contains(tau));
        Assert.IsTrue(parser.AddGrammarRule(CUSTOM_RULE, 0));
        Assert.IsTrue(parser.Grammar[0] == CUSTOM_RULE);
        Assert.IsTrue(parser.AddGrammarRule(tau, -1));
        Assert.IsTrue(parser.Grammar.Last() == tau);
        Assert.IsTrue(parser.GrammarContains(tau.Name));
        Assert.IsTrue(parser.RemoveGrammarRule(CUSTOM_RULE.Name));
        Assert.IsFalse(parser.GrammarContains(CUSTOM_RULE.Name));
        Assert.IsTrue(parser.RemoveGrammarRuleAt(parser.Grammar.Length - 1));
        Assert.IsFalse(parser.GrammarContains(tau.Name));
    }
}
