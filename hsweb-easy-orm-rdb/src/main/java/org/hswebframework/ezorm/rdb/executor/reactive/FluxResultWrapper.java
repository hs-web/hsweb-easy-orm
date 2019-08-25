package org.hswebframework.ezorm.rdb.executor.reactive;

import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapperContext;
import reactor.core.publisher.Flux;
import reactor.core.publisher.FluxSink;

public class FluxResultWrapper<E> implements ResultWrapper<E, Flux<E>> {

    private ResultWrapper<E, ?> wrapper;

    private FluxSink<E> fluxSink;

    private Flux<E> flux;

    public static void main(String[] args) {
        Flux flux= Flux.create(sink ->{
            System.out.println(sink);
            for (int i = 0; i < 100; i++) {

                sink.next(i);
            }
            sink.complete();
        });



//        flux.subscribe();
//        flux.subscribe(System.out::println);

    }

    public FluxResultWrapper(ResultWrapper<E, ?> wrapper) {
        this.flux = Flux.create(sink -> fluxSink = sink);
        this.flux.subscribe();

        this.wrapper=wrapper;
    }

    @Override
    public E newRowInstance() {
        return wrapper.newRowInstance();
    }

    @Override
    public void beforeWrap(ResultWrapperContext context) {
        wrapper.beforeWrap(context);
    }

    @Override
    public void wrapColumn(ColumnWrapperContext<E> context) {
        wrapper.wrapColumn(context);
    }

    @Override
    public boolean completedWrapRow(int rowIndex, E result) {

        fluxSink.next(result);

        return !fluxSink.isCancelled() && wrapper.completedWrapRow(rowIndex, result);
    }

    @Override
    public void completedWrap() {
        fluxSink.complete();
    }

    @Override
    public Flux<E> getResult() {
        return flux;
    }
}
