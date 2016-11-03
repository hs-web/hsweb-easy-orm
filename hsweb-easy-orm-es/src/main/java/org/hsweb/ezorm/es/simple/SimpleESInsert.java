package org.hsweb.ezorm.es.simple;

import org.hsweb.ezorm.core.Insert;
import org.hsweb.ezorm.es.ESInsert;
import org.hsweb.ezorm.es.ESTable;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * @author zhouhao
 */
class SimpleESInsert<T> implements ESInsert<T> {
    List<T>    data     = new ArrayList<>();
    ESTable<T> table    = null;

    public SimpleESInsert(ESTable<T> table) {
        this.table = table;
    }

    @Override
    public Insert<T> skipTrigger() {
        return this;
    }

    @Override
    public Insert<T> value(T data) {
        this.data.add(data);
        return this;
    }

    @Override
    public Insert<T> values(Collection<T> data) {
        this.data.addAll(data);
        return this;
    }

    @Override
    public int exec() throws SQLException {
        // TODO: 16-11-3
        return 0;
    }
}
