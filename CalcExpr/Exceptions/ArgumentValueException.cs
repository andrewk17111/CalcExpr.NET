namespace CalcExpr.Exceptions;

public class ArgumentValueException : Exception
{
    /// <summary>
    /// Initializes a new instance of the <see cref="ArgumentValueException"/> class using the specified invalid value.
    /// </summary>
    /// <param name="x">The value that is invalid.</param>
    public ArgumentValueException(double x) : base($"The value '{x}' is not a valid argument value for this operation.")
    { }
}
