package org.hswebframework.ezorm.rdb.operator.dml;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.Map;

@Getter
@Setter
@EqualsAndHashCode
public class FunctionColumn {
    private String column;

    private String function;

    private Map<String, Object> opts;

}
