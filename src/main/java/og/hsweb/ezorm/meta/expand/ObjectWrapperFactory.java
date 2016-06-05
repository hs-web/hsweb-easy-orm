package og.hsweb.ezorm.meta.expand;


import og.hsweb.ezorm.meta.TableMetaData;

public interface ObjectWrapperFactory {
    <T> ObjectWrapper<T> createObjectWrapper(TableMetaData metaData);
}
