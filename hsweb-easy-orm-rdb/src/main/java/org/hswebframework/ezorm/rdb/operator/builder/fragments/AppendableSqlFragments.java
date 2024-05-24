package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import java.util.Collection;

public interface AppendableSqlFragments extends SqlFragments {

    AppendableSqlFragments addSql(String... sql);

    AppendableSqlFragments addSql(Collection<String> sql);

    AppendableSqlFragments addParameter(Object... parameter);

    AppendableSqlFragments addParameter(Collection<?> parameter);

    AppendableSqlFragments addFragments(SqlFragments fragments);

    default AppendableSqlFragments add(SqlFragments fragments) {
        return addFragments(fragments);
    }

}
