namespace CalcExpr.Tokenization.Tokens;

public interface IToken
{
    public string Value { get; }

    public int Index { get; }

    public int Length => Value.Length;

    public char this[int index] => Value[index];

    public string this[Range range] => Value[range];
}
