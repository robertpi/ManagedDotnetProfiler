using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ManagedDotnetProfiler;

internal enum CorSaveSize
{
    cssAccurate = 0x0000,               // Find exact save size, accurate but slower.
    cssQuick = 0x0001,               // Estimate save size, may pad estimate, but faster.
    cssDiscardTransientCAs = 0x0002,               // remove all of the CAs of discardable types
}
