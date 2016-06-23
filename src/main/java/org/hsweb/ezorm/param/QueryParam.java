package org.hsweb.ezorm.param;

import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by 浩 on 2016-01-16 0016.
 */
public class QueryParam<C extends QueryParam> extends SqlParam<C> implements Serializable, Cloneable {
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
     */
    private List<Sort> sorts = new LinkedList<>();

    private boolean forUpdate = false;

    public C select(String... fields) {
        return (C) this.includes(fields);
    }

    public Sort<C> orderBy(String sortField) {
        Sort<C> sort = new Sort(this, sortField);
        sorts.add(sort);
        return sort;
    }

    public C doPaging(int pageIndex) {
        this.pageIndex = pageIndex;
        this.paging = true;
        return (C) this;
    }

    public C doPaging(int pageIndex, int pageSize) {
        this.pageIndex = pageIndex;
        this.pageSize = pageSize;
        this.paging = true;
        return (C) this;
    }

    public C rePaging(int total) {
        paging = true;
        // 当前页没有数据后跳转到最后一页
        if (this.getPageIndex() != 0 && (pageIndex * pageSize) >= total) {
            int tmp = total / this.getPageSize();
            pageIndex = total % this.getPageSize() == 0 ? tmp - 1 : tmp;
        }
        return (C) this;
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

    @Override
    public C clone() {
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
        return (C) sqlParam;
    }
}
