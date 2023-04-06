using System;
using ProfilerAbstractIL;

namespace ManagedDotnetProfiler;

internal class MethodBody
{
    public MethodBody(ReadOnlyMemory<byte> rawMethodBody, IL.ILMethodBody parsedMethodBody)
    {
        RawMethodBody = rawMethodBody;
        ParsedMethodBody = parsedMethodBody;
    }

    public ReadOnlyMemory<Byte> RawMethodBody { get; }
    public IL.ILMethodBody ParsedMethodBody { get; }
}