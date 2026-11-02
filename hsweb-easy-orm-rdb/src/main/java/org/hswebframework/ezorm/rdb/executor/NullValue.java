package org.hswebframework.ezorm.rdb.executor;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NonNull;
import org.hswebframework.ezorm.rdb.metadata.DataType;

@Getter
@AllArgsConstructor(staticName = "of")
public class NullValue {

    private Class<?> type;

    private DataType dataType;

    public static NullValue of(DataType dataType) {
        return of(dataType.getJavaType(), dataType);
    }

    public Class<?> getType() {
        return type == null ? dataType.getJavaType() : type;
    }

    @Override
    public String toString() {
        return "null" + (type != null ? "("+type.getSimpleName()+")" : (dataType != null ? "(" + dataType.getId() + ")" : ""));
    }
}
