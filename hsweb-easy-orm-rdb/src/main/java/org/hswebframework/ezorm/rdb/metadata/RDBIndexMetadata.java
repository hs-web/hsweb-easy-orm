package org.hswebframework.ezorm.rdb.metadata;

import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.core.meta.ObjectType;

import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * @author zhouhao
 * @since 1.0.0
 */
@Getter
@Setter
public class RDBIndexMetadata implements ObjectMetadata {

    private String name;

    private String tableName;

    private String alias;

    private String comment;

    private List<IndexColumn> columns = new LinkedList<>();

    private boolean unique;

    private boolean primaryKey;

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder(name)
                .append(" ")
                .append(unique ? "unique index" : "index")
                .append(" on ")
                .append(tableName);
        builder.append("(");
        int index = 0;
        for (IndexColumn column : columns) {
            if (index++ != 0) {
                builder.append(",");
            }
            builder.append(column.getColumn())
                    .append(" ")
                    .append(column.getSort() == IndexSort.non ? "" : column.getSort().name());
        }
        builder.append(")");

        return builder.toString();
    }

    public boolean contains(String column) {
        return columns.stream().anyMatch(indexColumn -> indexColumn.getColumn().equals(column));
    }

    @Override
    public ObjectType getObjectType() {
        return RDBObjectType.index;
    }

    @Override
    @SneakyThrows
    public RDBIndexMetadata clone() {
        RDBIndexMetadata metadata = (RDBIndexMetadata) super.clone();

        columns.stream()
                .map(IndexColumn::clone)
                .forEach(metadata.columns::add);

        return metadata;
    }

    public enum IndexSort {
        non, asc, desc
    }

    @Getter
    @Setter
    public static class IndexColumn implements Cloneable, Comparable<IndexColumn> {
        private String column;

        private IndexSort sort;

        private int sortIndex;

        public static IndexColumn of(String column, IndexSort sort) {
            IndexColumn indexColumn = new IndexColumn();
            indexColumn.setColumn(column);
            indexColumn.setSort(sort);
            return indexColumn;
        }

        @Override
        @SneakyThrows
        public IndexColumn clone() {
            return (IndexColumn) super.clone();
        }

        @Override
        public int compareTo(IndexColumn o) {
            return Integer.compare(sortIndex, o.sortIndex);
        }
    }
}
