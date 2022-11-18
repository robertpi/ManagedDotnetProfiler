using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static ProfilerAbstractIL.IL;

namespace ManagedDotnetProfiler;

internal class MethodBody
{
    public MethodBody(ReadOnlyMemory<byte> rawMethodBody, ILMethodBody parsedMethodBody)
    {
        RawMethodBody = rawMethodBody;
        ParsedMethodBody = parsedMethodBody;
    }

    public ReadOnlyMemory<Byte> RawMethodBody { get; }
    public ILMethodBody ParsedMethodBody { get; }
}
