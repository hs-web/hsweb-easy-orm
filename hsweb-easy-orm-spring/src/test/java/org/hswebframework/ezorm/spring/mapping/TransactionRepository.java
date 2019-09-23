package org.hswebframework.ezorm.spring.mapping;

import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.junit.Assert;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.stream.Collectors;


@Service
public class TransactionRepository {
    @Autowired
    ReactiveRepository<TestEntity, String> reactiveRepository;

    @Autowired
    SyncRepository<TestEntity, String> syncRepository;

    @Transactional
    public Mono<Integer> test() {

        return Flux.concat(
                reactiveRepository.insert(Mono.just(TestEntity.of("test", "name"))),
                reactiveRepository.createQuery().count(),
                reactiveRepository.createDelete()
                        .where(TestEntity::getId, "test")
                        .execute())
                .collect(Collectors.summingInt(Integer::intValue));
    }
}