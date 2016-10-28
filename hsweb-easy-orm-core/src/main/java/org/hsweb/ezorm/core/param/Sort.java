package org.hsweb.ezorm.core.param;

/**
 * 排序
 *
 * @author zhouhao
 * @since 1.0
 */
public class Sort extends Column {

    private String order = "asc";

    private transient QueryParam queryParam;

    public String getOrder() {
        return order;
    }

    public void setOrder(String order) {
        this.order = order;
    }

    public Sort() {
    }

    public Sort(QueryParam queryParam, String name) {
        this.queryParam = queryParam;
        setName(name);
    }

    public QueryParam asc() {
        this.order = "asc";
        return queryParam;
    }

    public QueryParam desc() {
        this.order = "desc";
        return queryParam;
    }

    public Sort and(String field) {
        return queryParam.orderBy(field);
    }

    @Override
    public int hashCode() {
        return String.valueOf(getName()).concat(order).hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) return false;
        return this.hashCode() == obj.hashCode();
    }

}
