using CalcExpr.Tokenization.Rules;
using System.Collections.Immutable;
using static CalcExpr.Tokenization.Tokens.Defaults.TokenizeFunctions;

namespace CalcExpr.Tokenization.Tokens.Defaults;

public static class TokenizerDefaults
{
    public static readonly ImmutableArray<ITokenizerRule> DefaultRules = [
        new CharSetRule("Symbol", ",=|∨⊕&∧≠<>≤≥*×/÷%^+-~¬!#∞∅", TokenizeSymbol),
        new CharSetRule("OpenBracket", "^([{", TokenizeOpenBracket),
        new CharSetRule("CloseBracket", ")]}", TokenizeCloseBracket),
        new RegexRule("Word", "^[A-Za-zΑ-Ωα-ω]+(_[A-Za-zΑ-Ωα-ω0-9]+)*", TokenizeWord),
        new RegexRule("Number", @"^((\d+\.?\d*)|(\d*\.?\d+))", TokenizeNumber),
    ];
}
