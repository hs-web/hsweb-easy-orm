package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;


@Getter
@Setter
@NoArgsConstructor(staticName = "of")
public class PrepareSqlFragments implements SqlFragments {

    @Override
    public boolean isEmpty() {
        return sql.isEmpty();
    }

    private List<String> sql = new ArrayList<>(64);

    private List<Object> parameters = new ArrayList<>(8);


    public PrepareSqlFragments addSql(String... sql) {
        this.sql.addAll(Arrays.asList(sql));
        return this;
    }

    public PrepareSqlFragments addParameter(Collection<Object> parameter) {
        this.parameters.addAll(parameter);
        return this;
    }

    public PrepareSqlFragments addParameter(Object... parameter) {
        return this.addParameter(Arrays.asList(parameter));
    }

}
