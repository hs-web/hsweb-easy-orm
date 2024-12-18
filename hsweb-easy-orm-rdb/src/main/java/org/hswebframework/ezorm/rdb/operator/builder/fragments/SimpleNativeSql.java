package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;

@Getter
//@Setter
@AllArgsConstructor
@NoArgsConstructor
class SimpleNativeSql implements NativeSql, Serializable {
    private static final long serialVersionUID = 1L;

    private String sql;
    private Object[] parameters;
}
