package org.hswebframework.ezorm.rdb.operator.dml;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.LinkedHashMap;
import java.util.Map;

@Getter
@Setter
@EqualsAndHashCode
public class FunctionColumn implements Serializable {
    private static final long serialVersionUID = 1L;

    private String column;

    private String function;

    private Map<String, Object> opts;

    @Override
    public String toString() {
        return function + "(" + column + ")";
    }

    public FunctionColumn option(String opt, Object value) {
        if (opts == null) {
            opts = new LinkedHashMap<>();
        }
        opts.put(opt, value);
        return this;
    }
}
