package og.hsweb.ezorm.render;

import og.hsweb.ezorm.meta.FieldMetaData;
import og.hsweb.ezorm.param.Term;
import og.hsweb.ezorm.render.dialect.DefaultDialect;

/**
 * Created by zhouhao on 16-5-17.
 */
public interface Dialect {
    Dialect MYSQL = new DefaultDialect() {
        @Override
        public String getQuoteStart() {
            return "`";
        }

        @Override
        public String getQuoteEnd() {
            return "`";
        }

        @Override
        public String doPaging(String sql, int pageIndex, int pageSize) {
            return new StringBuilder(sql)
                    .append(" LIMIT ")
                    .append(pageSize * pageIndex)
                    .append(",")
                    .append(pageSize * (pageIndex + 1)).toString();
        }
    };

    Dialect ORACLE = new DefaultDialect() {
        @Override
        public String getQuoteStart() {
            return "\"";
        }

        @Override
        public String getQuoteEnd() {
            return "\"";
        }

        @Override
        public String doPaging(String sql, int pageIndex, int pageSize) {
            return new StringBuilder()
                    .append("SELECT * FROM ( SELECT row_.*, rownum rownum_ FROM (")
                    .append(sql)
                    .append(") row_ )")
                    .append("WHERE rownum_ <= ")
                    .append(pageSize * (pageIndex + 1))
                    .append(" AND rownum_ > ")
                    .append(pageSize * pageIndex).toString();
        }
    };

    Dialect H2 = ORACLE;

    String getQuoteStart();

    String getQuoteEnd();

    String wrapperWhere(String wherePrefix, Term term, FieldMetaData fieldMetaData, String tableAlias);

    String doPaging(String sql, int pageIndex, int pageSize);
}
