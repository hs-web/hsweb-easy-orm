package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.apache.commons.collections4.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;


@Getter
@Setter
@NoArgsConstructor(staticName = "of")
@AllArgsConstructor(staticName = "of")
public class PrepareSqlFragments implements AppendableSqlFragments {

    public static PrepareSqlFragments of(String sql, Object... parameter) {
        return PrepareSqlFragments.of()
                                  .addSql(sql)
                                  .addParameter(parameter);
    }

    @Override
    public boolean isEmpty() {
        return sql.isEmpty();
    }

    private List<String> sql = new ArrayList<>(64);

    private List<Object> parameters = new ArrayList<>(16);

    public void removeLastSql() {
        if (sql.isEmpty()) {
            return;
        }
        sql.remove(sql.size() - 1);
    }

    public PrepareSqlFragments addFragments(SqlFragments fragments) {

        return addSql(fragments.getSql())
            .addParameter(fragments.getParameters());
    }

    public PrepareSqlFragments addSql(String... sql) {
        for (String _sql : sql) {
            if (null != _sql) {
                this.sql.add(_sql);
            }
        }
        return this;
    }

    @SuppressWarnings("all")
    public PrepareSqlFragments addSql(Collection<String> sql) {
        for (String s : sql) {
            this.sql.add(s);
        }
        return this;
    }

    @SuppressWarnings("all")
    public PrepareSqlFragments addParameter(Collection<?> parameter) {
        if (CollectionUtils.isEmpty(parameter)) {
            return this;
        }
        for (Object o : parameter) {
            this.parameters.add(o);
        }
        return this;
    }

    @SuppressWarnings("all")
    public PrepareSqlFragments addParameter(Object... parameter) {
        if (parameter == null || parameter.length == 0) {
            return this;
        }
        for (Object o : parameter) {
            this.parameters.add(o);
        }
        return this;
    }

    @Override
    public String toString() {
        return toRequest().getSql();
    }
}
