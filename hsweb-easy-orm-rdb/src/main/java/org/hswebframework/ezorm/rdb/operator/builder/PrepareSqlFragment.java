package org.hswebframework.ezorm.rdb.operator.builder;

import lombok.Getter;
import lombok.Setter;

import java.util.Collection;


@Getter
@Setter
public class PrepareSqlFragment {

    private String sql;

    private Collection<Object> parameters;

}
