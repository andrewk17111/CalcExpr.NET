namespace CalcExpr.Exceptions;

public class UnknownValueException : Exception
{
    /// <summary>
    /// Initializes a new instance of the <see cref="UnknownValueException"/> class using the specified 
    /// <see cref="Variable"/> name.
    /// </summary>
    /// <param name="variable">The name of the variable with the unkown value.</param>
    public UnknownValueException(string variable)
        : base($"A value for the variable '{variable}' was not provided.")
    { }
}
