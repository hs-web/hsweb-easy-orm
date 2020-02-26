package org.hswebframework.ezorm.rdb.executor;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NonNull;
import org.hswebframework.ezorm.rdb.metadata.DataType;

@Getter
@AllArgsConstructor(staticName = "of")
public class NullValue {
    @Deprecated
    private Class type;

    @NonNull
    private DataType dataType;

    public static NullValue of(DataType dataType){
        return of(dataType.getJavaType(),dataType);
    }

    @Override
    public String toString() {
        return "null" + (dataType==null?"": (type != null ? "(" + dataType.getId() + ")" : ""));
    }
}
