using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ManagedDotnetProfiler;

[GenerateNativeStub]
public unsafe interface IMethodMalloc : IUnknown
{
    byte* Alloc(ulong cb);
}
