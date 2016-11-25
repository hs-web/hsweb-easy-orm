package org.hsweb.ezorm.rdb.render.support.simple;

import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.meta.Correlation;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.core.param.Param;
import org.hsweb.ezorm.rdb.render.SqlRender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public abstract class CommonSqlRender<R extends Param> implements SqlRender<R> {
    protected Logger logger = LoggerFactory.getLogger(this.getClass());

    public class OperationColumn {
        private String            tableName;
        private RDBColumnMetaData RDBColumnMetaData;

        public OperationColumn(String tableName, RDBColumnMetaData RDBColumnMetaData) {
            this.tableName = tableName;
            this.RDBColumnMetaData = RDBColumnMetaData;
            if (tableName == null) this.tableName = RDBColumnMetaData.getTableMetaData().getAlias();
        }

        public String getTableName() {
            return tableName;
        }

        public RDBColumnMetaData getRDBColumnMetaData() {
            return RDBColumnMetaData;
        }
    }

    public List<OperationColumn> parseOperationField(RDBTableMetaData metaData, R param) {
        Set<String> includes = param.getIncludes(),
                excludes = param.getExcludes();
        boolean includesIsEmpty = includes.isEmpty(),
                excludesIsEmpty = excludes.isEmpty();
        List<OperationColumn> tmp = new ArrayList<>();
        if ((includesIsEmpty && excludesIsEmpty)) {
            metaData.getColumns().forEach(column -> tmp.add(new OperationColumn(null, column)));
            return tmp;
        }
        //指定了exclude,没有指定include
        if (includesIsEmpty && !excludesIsEmpty) {
            boolean hasSelf = false;
            for (String exclude : excludes) {
                if (exclude.contains(".")) {
                    includes.add(exclude.split("[.]")[0] + ".*");
                } else {
                    hasSelf = true;
                }
            }
            if (hasSelf) includes.add("*");
            includesIsEmpty = false;
        }
        if (!includesIsEmpty) {
            includes.forEach(include -> {
                if (excludes.contains(include)) return;
                if ("*".equals(include)) {
                    metaData.getColumns().forEach(column -> {
                        if (excludes.contains(column.getAlias()) || excludes.contains(column.getName()))
                            return;
                        tmp.add(new OperationColumn(null, column));
                    });
                    return;
                }
                if (include.contains(".")) {
                    String[] columnInfo = include.split("[.]");
                    RDBTableMetaData table = metaData.getDatabaseMetaData().getTableMetaData(columnInfo[0]);
                    String tname = null;
                    if (null == table) {
                        Correlation correlation = metaData.getCorrelation(columnInfo[0]);
                        if (correlation != null) {
                            table = metaData.getDatabaseMetaData().getTableMetaData(correlation.getTargetTable());
                            tname = correlation.getAlias();
                        }
                    } else {
                        tname = table.getAlias();
                    }
                    if (null == table) return;
                    if (columnInfo[1].equals("*")) {
                        String finalName = tname;
                        table.getColumns().forEach(column -> {
                            if (excludes.contains(column.getFullAliasName()) || excludes.contains(column.getFullName())
                                    || excludes.contains(finalName + "." + column.getName())
                                    || excludes.contains(finalName + "." + column.getAlias()))
                                return;
                            tmp.add(new OperationColumn(finalName, column));
                        });
                        return;
                    } else {
                        RDBColumnMetaData column = metaData.findColumn(include);
                        if (null != column) {
                            if (excludes.contains(column.getFullAliasName()) || excludes.contains(column.getFullName()))
                                return;
                            tmp.add(new OperationColumn(tname, column));
                        }
                    }
                } else {
                    RDBColumnMetaData column = metaData.findColumn(include);
                    if (null != column) {
                        if (excludes.contains(column.getAlias()) || excludes.contains(column.getName()))
                            return;
                        tmp.add(new OperationColumn(column.getTableMetaData().getAlias(), column));
                    }
                }
            });
        }
        if (tmp.isEmpty()) throw new UnsupportedOperationException("未找到任何查询字段!");
        return tmp;
    }
}
