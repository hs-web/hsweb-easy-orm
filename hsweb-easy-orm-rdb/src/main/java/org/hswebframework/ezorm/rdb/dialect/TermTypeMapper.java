package org.hswebframework.ezorm.rdb.dialect;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SimpleSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlTemplateParser;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetaData;
import org.hswebframework.ezorm.rdb.render.SqlAppender;

public interface TermTypeMapper {
    SqlAppender accept(String wherePrefix, Term term, RDBColumnMetaData column, String tableAlias);

    static TermTypeMapper sql(String sql) {
        return (wherePrefix, term, column, tableAlias) -> new SqlAppender(sql);
    }

    static TermTypeMapper sql(String sql, Object param) {

        return (wherePrefix, term, column, tableAlias) -> {
            SqlRequest request = SqlTemplateParser.parse(sql, param);

            return new SqlAppender(((SimpleSqlRequest) request).toNativeSql());
        };
    }

}
