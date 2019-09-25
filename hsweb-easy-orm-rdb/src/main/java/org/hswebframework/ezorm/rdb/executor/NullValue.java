package org.hswebframework.ezorm.rdb.executor;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.rdb.metadata.DataType;

@Getter
@AllArgsConstructor(staticName = "of")
public class NullValue {
    private Class type;

    private DataType dataType;

    @Override
    public String toString() {
        return "null" + (dataType==null?"": (type != null ? "(" + dataType.getId() + ")" : ""));
    }
}
