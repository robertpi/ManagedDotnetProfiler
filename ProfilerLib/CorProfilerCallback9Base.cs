﻿namespace ProfilerLib
{
    public abstract class CorProfilerCallback9Base : CorProfilerCallback8Base, Interfaces.ICorProfilerCallback9
    {
        private readonly NativeObjects.ICorProfilerCallback9 _corProfilerCallback9;

        protected CorProfilerCallback9Base()
        {
            _corProfilerCallback9 = NativeObjects.ICorProfilerCallback9.Wrap(this);
        }

        protected override HResult QueryInterface(in Guid guid, out nint ptr)
        {
            if (guid == Interfaces.ICorProfilerCallback9.Guid)
            {
                ptr = _corProfilerCallback9;
                return HResult.S_OK;
            }

            return base.QueryInterface(guid, out ptr);
        }

        #region ICorProfilerCallback9

        HResult Interfaces.ICorProfilerCallback9.DynamicMethodUnloaded(FunctionId functionId)
        {
            return DynamicMethodUnloaded(functionId);
        }

        #endregion

        protected virtual HResult DynamicMethodUnloaded(FunctionId functionId)
        {
            return HResult.E_NOTIMPL;
        }
    }
}
