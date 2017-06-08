package org.hsweb.ezorm.rdb.render.support.sqlserver;

import org.hsweb.ezorm.core.param.QueryParam;
import org.hsweb.ezorm.core.param.Sort;
import org.hsweb.ezorm.rdb.executor.SQL;
import org.hsweb.ezorm.rdb.meta.Correlation;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.render.SqlAppender;
import org.hsweb.ezorm.rdb.render.dialect.Dialect;
import org.hsweb.ezorm.rdb.render.support.simple.CommonSqlRender;
import org.hsweb.ezorm.rdb.render.support.simple.SimpleSQL;
import org.hsweb.ezorm.rdb.render.support.simple.SimpleWhereSqlBuilder;

import java.util.*;

/**
 * Created by zhouhao on 16-5-17.
 */
public class SqlServerSelectSqlRender extends CommonSqlRender<QueryParam> {

    private Dialect dialect;

    public SqlServerSelectSqlRender(Dialect dialect) {
        this.dialect = dialect;
    }

    public Dialect getDialect() {
        return dialect;
    }

    public void setDialect(Dialect dialect) {
        this.dialect = dialect;
    }

    class SimpleSelectSqlRenderProcess extends SimpleWhereSqlBuilder {
        private RDBTableMetaData      metaData;
        private QueryParam            param;
        private List<OperationColumn> selectField;
        private SqlAppender whereSql        = new SqlAppender();
        private Set<String> needSelectTable = new LinkedHashSet<>();
        private List<Sort>  sorts           = new ArrayList<>();

        public SimpleSelectSqlRenderProcess(RDBTableMetaData metaData, QueryParam param) {
            this.metaData = metaData;
            this.param = param;
            if (param.getIncludes().isEmpty() && param.getExcludes().isEmpty()) {
                param.includes("*");
                metaData.getCorrelations().forEach(correlation -> param.includes(correlation.getAlias() + ".*"));
            }
            //解析要查询的字段
            this.selectField = parseOperationField(metaData, param);
            //解析查询条件
            buildWhere(metaData, "", param.getTerms(), whereSql, needSelectTable);
            if (!whereSql.isEmpty()) whereSql.removeFirst();
            //加入要查询的表
            this.selectField.stream().forEach(field -> {
                needSelectTable.add(field.getTableName());
            });
            param.getSorts().forEach(sort -> {
                RDBColumnMetaData rDBColumnMetaData = metaData.findColumn(sort.getName());
                if (rDBColumnMetaData.getName() == null) return;
                String tableName = getTableAlias(metaData, sort.getName());
                needSelectTable.add(tableName);
                sort.setName(getDialect().buildColumnName(tableName, rDBColumnMetaData.getName()));
                sorts.add(sort);
            });
        }

        public SQL process() {
            boolean doPaing = param.isPaging() && !param.isForUpdate();
            SqlAppender appender = new SqlAppender();
            if (doPaing) {
                //SELECT w2.n, w1.* FROM s_test w1,(
//                    SELECT TOP 1030 row_number() over (ORDER BY u_id DESC) n,* FROM s_test
//              ) w2 WHERE w1.u_id = w2.u_id AND w2.n > 0 ORDER BY w2.n ASC

                String defaultOrder;
                if (sorts.isEmpty()) {
                    defaultOrder = metaData
                            .getProperty("default-order", metaData.getColumns().stream().findAny().get().getAlias()).getValue();
                } else {
                    defaultOrder = sorts.stream()
                            .map(sort -> sort.getName().concat(" ").concat(sort.getOrder()))
                            .reduce((s1, s2) -> s1.concat(",").concat(s2)).get();
                }
                appender.add("SELECT tmp_.* FROM ", metaData.getName(), " t ,"
                        , "(SELECT TOP ((#{pageIndex}+1)*#{pageSize}) row_number() over (ORDER BY ", defaultOrder, " ) rownum_,");
            } else {
                appender.add("SELECT ");
            }

            if (selectField.isEmpty()) appender.add(" * ");
            selectField.forEach(operationColumn -> {
                RDBColumnMetaData rDBColumnMetaData = operationColumn.getRDBColumnMetaData();
                String tableName = rDBColumnMetaData.getTableMetaData().getName();
                Correlation correlation = metaData.getCorrelation(tableName);
                if (correlation == null) {
                    appender.add(getDialect().buildColumnName(operationColumn.getTableName(), rDBColumnMetaData.getName())
                            , " AS "
                            , dialect.getQuoteStart()
                            , rDBColumnMetaData.getAlias()
                            , dialect.getQuoteEnd());
                } else {
                    //关联的另外一张表
                    if (correlation.isOne2one()) {
                        appender.add(getDialect().buildColumnName(operationColumn.getTableName(), rDBColumnMetaData.getName())
                                , " AS "
                                , dialect.getQuoteStart()
                                , operationColumn.getTableName(), ".", rDBColumnMetaData.getAlias()
                                , dialect.getQuoteEnd());
                    }
                }
                appender.add(",");
            });
            appender.removeLast();

            appender.add(" FROM ", metaData.getName(), " ", metaData.getAlias());

            //生成join
            needSelectTable.stream()
                    .filter(table -> !table.equals(metaData.getName()) && metaData.getCorrelation(table) != null)
                    .map(table -> metaData.getCorrelation(table))
                    .sorted()
                    .forEach(correlation -> {
                        appender.addSpc("", correlation.getJoin(), correlation.getTargetTable(), correlation.getAlias(), "ON");
                        SqlAppender joinOn = new SqlAppender();
                        buildWhere(metaData.getDatabaseMetaData().getTableMetaData(correlation.getTargetTable()), "", correlation.getTerms(), joinOn, new HashSet());
                        if (!joinOn.isEmpty()) joinOn.removeFirst();
                        appender.addAll(joinOn);
                    });
            if (!whereSql.isEmpty())
                appender.add(" WHERE ", "").addAll(whereSql);

            if (doPaing) {
                String uniqueColumn = metaData
                        .getProperty("unique-column", () -> {
                            RDBColumnMetaData column = metaData.findColumn("u_id") == null ? metaData.findColumn("id") : null;
                            if (column != null) return column.getName();
                            return null;
                        })
                        .getValue();
                if (uniqueColumn == null) {
                    throw new NullPointerException("unique column is null,you can set table property:unique-column or add column : id or u_id");
                }
                appender.add(") tmp_ WHERE t.[", uniqueColumn, "] = tmp_.[" + uniqueColumn + "] AND tmp_.rownum_ > (#{pageIndex}*#{pageSize}) ORDER BY tmp_.rownum_ ASC");

            } else if (!sorts.isEmpty()) {
                appender.add(" ORDER BY ");
                sorts.forEach(sort -> appender.add(sort.getName(), " ", sort.getOrder(), ","));
                appender.removeLast();
            }
            String sql = appender.toString();
//            if (param.isPaging() && !param.isForUpdate()) {
//                sql = dialect.doPaging(sql, param.getPageIndex(), param.getPageSize());
//            }
            if (param.isForUpdate()) sql.concat(" FOR UPDATE");
            SimpleSQL simpleSQL = new SimpleSQL(sql, param);
            return simpleSQL;
        }

        @Override
        public Dialect getDialect() {
            return dialect;
        }
    }

    @Override
    public SQL render(RDBTableMetaData metaData, QueryParam param) {
        return new SimpleSelectSqlRenderProcess(metaData, param).process();
    }
}
