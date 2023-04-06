﻿namespace ProfilerLib
{
    public abstract class CorProfilerCallback7Base : CorProfilerCallback6Base, Interfaces.ICorProfilerCallback7
    {
        private readonly NativeObjects.ICorProfilerCallback7 _corProfilerCallback7;

        protected CorProfilerCallback7Base()
        {
            _corProfilerCallback7 = NativeObjects.ICorProfilerCallback7.Wrap(this);
        }

        protected override HResult QueryInterface(in Guid guid, out nint ptr)
        {
            if (guid == Interfaces.ICorProfilerCallback7.Guid)
            {
                ptr = _corProfilerCallback7;
                return HResult.S_OK;
            }

            return base.QueryInterface(guid, out ptr);
        }

        #region ICorProfilerCallback7

        HResult Interfaces.ICorProfilerCallback7.ModuleInMemorySymbolsUpdated(ModuleId moduleId)
        {
            return ModuleInMemorySymbolsUpdated(moduleId);
        }

        #endregion

        protected virtual HResult ModuleInMemorySymbolsUpdated(ModuleId moduleId)
        {
            return HResult.E_NOTIMPL;
        }
    }
}
