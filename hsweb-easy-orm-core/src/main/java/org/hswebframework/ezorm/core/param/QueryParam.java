package org.hswebframework.ezorm.core.param;

import io.swagger.v3.oas.annotations.Hidden;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 查询参数
 *
 * @author zhouhao
 * @since 1.0
 */
@Getter
@Setter
@SuppressWarnings("all")
public class QueryParam extends Param implements Serializable, Cloneable {
    private static final long serialVersionUID = 7941767360194797891L;

    public static final int DEFAULT_FIRST_PAGE_INDEX = Integer.getInteger("easyorm.page.fist.index", 0);

    public static final int DEFAULT_PAGE_SIZE = Integer.getInteger("easyorm.page.size", 25);

    /**
     * 是否进行分页，默认为true
     */
    @Schema(description = "是否分页")
    private boolean paging = true;

    /**
     * 第一页索引
     *
     * @since 3.0.3
     */
    @Getter
    @Schema(description = "第一页索引")
    private int firstPageIndex = DEFAULT_FIRST_PAGE_INDEX;

    /**
     * 第几页
     */
    @Schema(description = "页码")
    private int pageIndex = firstPageIndex;

    /**
     * 每页显示记录条数
     */
    @Schema(description = "每页数量")
    private int pageSize = DEFAULT_PAGE_SIZE;

    /**
     * 排序字段
     *
     * @since 1.0
     */
    private List<Sort> sorts = new LinkedList<>();

    @Hidden
    private transient int pageIndexTmp = 0;

    @Hidden
    private boolean forUpdate = false;

    @Schema(description = "上下文信息")
    private Map<String, Object> context;

    public Optional<Object> getContext(String key) {
        if (context == null) {
            return Optional.empty();
        }
        return Optional.ofNullable(context.get(key));
    }

    public void context(String key, Object value) {
        if (context == null) {
            context = new HashMap<>();
        }
        context.put(key, value);
    }

    public Sort orderBy(String column) {
        Sort sort = new Sort(column);
        sorts.add(sort);
        return sort;
    }

    public <Q extends QueryParam> Q noPaging() {
        setPaging(false);
        return (Q) this;
    }

    public <Q extends QueryParam> Q doPaging(int pageIndex) {
        setPageIndex(pageIndex);
        setPaging(true);
        return (Q) this;
    }

    public <Q extends QueryParam> Q doPaging(int pageIndex, int pageSize) {
        setPageIndex(pageIndex);
        setPageSize(pageSize);
        setPaging(true);
        return (Q) this;
    }

    public <Q extends QueryParam> Q rePaging(int total) {
        paging = true;
        // 当前页没有数据后跳转到最后一页
        if (pageIndex != 0 && (pageIndex * pageSize) >= total) {
            int tmp = total / this.getPageSize();
            pageIndex = total % this.getPageSize() == 0 ? tmp - 1 : tmp;
        }
        return (Q) this;
    }

    public void setPageIndex(int pageIndex) {
        this.pageIndex = Math.max(pageIndex - firstPageIndex, 0);
        this.pageIndexTmp = this.pageIndex;
    }

    public void setFirstPageIndex(int firstPageIndex) {

        this.pageIndex = this.pageIndexTmp = Math.max(this.pageIndexTmp - this.firstPageIndex - firstPageIndex, 0);
        this.firstPageIndex = firstPageIndex;
    }

    public int getThinkPageIndex() {
        return this.pageIndex + firstPageIndex;
    }

    @Override
    public QueryParam clone() {
        QueryParam queryParam = ((QueryParam) super.clone());
        if (queryParam.context != null) {
            queryParam.context = new HashMap<>(queryParam.context);
        }
        return queryParam;
    }
}
