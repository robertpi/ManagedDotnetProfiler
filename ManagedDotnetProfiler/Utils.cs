namespace ManagedDotnetProfiler;
internal unsafe static class Utils
{
    public static byte[] BufferToArray(byte* inBuffer, uint length)
    {
        var outArray = new byte[length];
        for (int i = 0; i < length; i++)
        {
            outArray[i] = inBuffer[i];
        }

        return outArray;
    }

    public static void ArrayToBuffer(byte[] inArray, byte* outBuffer)
    {
        for (int i = 0; i < inArray.Length; i++)
        {
            outBuffer[i] = inArray[i];
        }
    }
}
