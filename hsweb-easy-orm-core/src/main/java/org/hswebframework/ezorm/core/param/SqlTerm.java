package org.hswebframework.ezorm.core.param;

import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;

/**
 * 直接拼接sql的方式
 *
 * @author zhouhao
 * @since 3.0
 */
@Getter
@Setter
public class SqlTerm extends Term {

    private String sql;

    public SqlTerm() {
    }

    public SqlTerm(String sql, Object... value) {
        this.sql = sql;
        setValue(value);
    }

    @Override
    @SneakyThrows
    public SqlTerm clone() {
        SqlTerm term = (SqlTerm) super.clone();
        term.setSql(getSql());
        return term;
    }
}
