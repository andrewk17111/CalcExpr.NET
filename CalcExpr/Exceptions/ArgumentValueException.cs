namespace CalcExpr.Exceptions;

public class ArgumentValueException : Exception
{
    public ArgumentValueException(double x) : base($"The value '{x}' is not a valid argument value for this operation.")
    { }
}
