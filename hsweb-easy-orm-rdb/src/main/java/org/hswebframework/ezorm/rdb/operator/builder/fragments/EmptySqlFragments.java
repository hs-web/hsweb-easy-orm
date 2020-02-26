package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.Collections;
import java.util.List;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class EmptySqlFragments implements SqlFragments {

    public static final EmptySqlFragments INSTANCE = new EmptySqlFragments();

    @Override
    public boolean isEmpty() {
        return true;
    }

    @Override
    public List<String> getSql() {
        return Collections.emptyList();
    }

    @Override
    public List<Object> getParameters() {
        return Collections.emptyList();
    }


}
