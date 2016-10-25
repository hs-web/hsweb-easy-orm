package org.hsweb.ezorm.param;

import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by 浩 on 2016-01-16 0016.
 */
public class QueryParam extends SqlParam implements Serializable, Cloneable {
    private static final long serialVersionUID = 7941767360194797891L;

    /**
     * 是否进行分页，默认为true
     */
    private boolean paging = true;

    /**
     * 第几页 从0开始
     */
    private int pageIndex = 0;

    /**
     * 每页显示记录条数
     */
    private int pageSize = 25;


    /**
     * 排序字段
     *
     * @since 1.0
     */
    private List<Sort> sorts = new LinkedList<>();

    /**
     * 自定义列,指定此数据时,includes失效
     *
     * @see this#includes
     * @since 1.1
     */
    private List<Column> columns = new LinkedList<>();

    /**
     * group by
     *
     * @since 1.1
     */
    private List<Column> groupBy = new LinkedList<>();

    /**
     * having
     *
     * @since 1.1
     */
    private List<Column> having = new LinkedList<>();

    private boolean forUpdate = false;

    public <Q extends QueryParam> Q select(String... fields) {
        return this.includes(fields);
    }

    public <Q extends QueryParam> Sort<Q> orderBy(String column) {
        Sort<Q> sort = new Sort(this, column);
        sorts.add(sort);
        return sort;
    }

    public <Q extends QueryParam> Sort<Q> orderBy(Column column) {
        Sort<Q> sort = new Sort(this, column);
        sorts.add(sort);
        return sort;
    }

    public <Q extends QueryParam> Q gourpBy(String column) {
        groupBy.add(Column.build(column));
        return (Q) this;
    }

    public <Q extends QueryParam> Q having(String column) {
        having.add(Column.build(column));
        return (Q) this;
    }


    public <Q extends QueryParam> Q doPaging(int pageIndex) {
        this.pageIndex = pageIndex;
        this.paging = true;
        return (Q) this;
    }

    public <Q extends QueryParam> Q doPaging(int pageIndex, int pageSize) {
        this.pageIndex = pageIndex;
        this.pageSize = pageSize;
        this.paging = true;
        return (Q) this;
    }

    public <Q extends QueryParam> Q rePaging(int total) {
        paging = true;
        // 当前页没有数据后跳转到最后一页
        if (this.getPageIndex() != 0 && (pageIndex * pageSize) >= total) {
            int tmp = total / this.getPageSize();
            pageIndex = total % this.getPageSize() == 0 ? tmp - 1 : tmp;
        }
        return (Q) this;
    }

    public boolean isPaging() {
        return paging;
    }

    public void setPaging(boolean paging) {
        this.paging = paging;
    }

    public int getPageIndex() {
        return pageIndex;
    }

    public void setPageIndex(int pageIndex) {
        this.pageIndex = pageIndex;
    }

    public int getPageSize() {
        return pageSize;
    }

    public void setPageSize(int pageSize) {
        this.pageSize = pageSize;
    }

    public List<Sort> getSorts() {
        return sorts;
    }

    public void setSorts(List<Sort> sorts) {
        this.sorts = sorts;
    }

    public void setForUpdate(boolean forUpdate) {
        this.forUpdate = forUpdate;
    }

    public boolean isForUpdate() {
        return forUpdate;
    }

    public List<Column> getColumns() {
        return columns;
    }

    public void setColumns(List<Column> columns) {
        this.columns = columns;
    }

    public List<Column> getGroupBy() {
        return groupBy;
    }

    public void setGroupBy(List<Column> groupBy) {
        this.groupBy = groupBy;
    }

    public List<Column> getHaving() {
        return having;
    }

    public void setHaving(List<Column> having) {
        this.having = having;
    }

    @Override
    public QueryParam clone() {
        QueryParam sqlParam = new QueryParam();
        sqlParam.setExcludes(new LinkedHashSet<>(excludes));
        sqlParam.setIncludes(new LinkedHashSet<>(includes));
        List<Term> terms = this.terms.stream().map(term -> term.clone()).collect(Collectors.toList());
        sqlParam.setTerms(terms);
        sqlParam.setPageIndex(pageIndex);
        sqlParam.setPageSize(pageSize);
        sqlParam.setPaging(paging);
        sqlParam.setSorts(sorts);
        sqlParam.setForUpdate(forUpdate);
        sqlParam.setColumns(columns);
        sqlParam.setGroupBy(groupBy);
        sqlParam.setHaving(having);
        return sqlParam;
    }
}
