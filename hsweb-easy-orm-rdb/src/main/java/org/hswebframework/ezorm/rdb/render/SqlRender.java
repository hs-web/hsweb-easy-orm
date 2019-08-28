package org.hswebframework.ezorm.rdb.render;

import org.hswebframework.ezorm.core.meta.ObjectMetaData;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;

public interface SqlRender<M extends ObjectMetaData, P> {

    ObjectType getObjectType();

    RenderType getRenderType();

    SqlRequest render(M metadata, P parameter);

}
