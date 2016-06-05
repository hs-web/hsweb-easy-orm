package og.hsweb.ezorm.param;

import org.apache.commons.beanutils.BeanUtils;

import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * Created by 浩 on 2016-01-16 0016.
 */
public class QueryParam extends SqlParam<QueryParam> implements Serializable, Cloneable {
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
    private Set<Sort> sorts = new LinkedHashSet<>();

    public QueryParam select(String... fields) {
        return this.includes(fields);
    }

    public Sort orderBy(String sortField) {
        Sort sort = new Sort(this, sortField);
        sorts.add(sort);
        return sort;
    }

    public QueryParam doPaging(int pageIndex) {
        this.pageIndex = pageIndex;
        this.paging = true;
        return this;
    }

    public QueryParam doPaging(int pageIndex, int pageSize) {
        this.pageIndex = pageIndex;
        this.pageSize = pageSize;
        this.paging = true;
        return this;
    }

    public QueryParam rePaging(int total) {
        paging = true;
        // 当前页没有数据后跳转到最后一页
        if (this.getPageIndex() != 0 && (pageIndex * pageSize) >= total) {
            int tmp = total / this.getPageSize();
            pageIndex = total % this.getPageSize() == 0 ? tmp - 1 : tmp;
        }
        return this;
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

    public static QueryParam newInstance() {
        return new QueryParam();
    }

    public Set<Sort> getSorts() {
        return sorts;
    }

    public void setSorts(Set<Sort> sorts) {
        this.sorts = sorts;
    }

    @Override
    public QueryParam clone() throws RuntimeException {
        try {
            return ((QueryParam) BeanUtils.cloneBean(this));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
