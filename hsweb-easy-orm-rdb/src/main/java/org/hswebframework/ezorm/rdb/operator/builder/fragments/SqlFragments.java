package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import java.util.List;

public interface SqlFragments {

    boolean isEmpty();

    List<String> getSql();

    List<Object> getParameters();
}
