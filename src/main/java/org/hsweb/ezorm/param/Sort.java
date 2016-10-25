package org.hsweb.ezorm.param;

/**
 * 排序
 *
 * @author zhouhao
 * @since 1.0
 */
public class Sort<Q extends QueryParam> {
    @Deprecated
    private String name;

    /**
     * 自定义列
     *
     * @see this#name
     * @since 1.1
     */
    private Column column;

    private String dir = "asc";

    private transient Q queryParam;

    @Deprecated
    public String getName() {
        return name;
    }

    @Deprecated
    public void setName(String name) {
        this.name = name;
    }

    public String getDir() {
        return dir;
    }

    public void setDir(String dir) {
        this.dir = dir;
    }

    public Sort() {
    }

    @Deprecated
    public Sort(Q queryParam, String name) {
        this.queryParam = queryParam;
        this.name = name;
    }

    public Sort(Q queryParam, Column column) {
        this.queryParam = queryParam;
        this.column = column;
    }

    public Column getColumn() {
        if (column == null) {
            column = Column.build(getName());
        }
        return column;
    }

    public void setColumn(Column column) {
        this.column = column;
    }

    public Sort(String name) {
        this.name = name;
    }

    public Q asc() {
        this.dir = "asc";
        return queryParam;
    }

    public Q desc() {
        this.dir = "desc";
        return queryParam;
    }

    public Sort<Q> and(String field) {
        return queryParam.orderBy(field);
    }

    @Override
    public int hashCode() {
        return String.valueOf(name).concat(dir).hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) return false;
        return this.hashCode() == obj.hashCode();
    }

}
