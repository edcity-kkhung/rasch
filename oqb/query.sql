====== SQL : Count number of trial and questionfor each paper_id with force_answer = 1 and published = 1 ======
SELECT
	p.id as paper_id,
    p.num_submitted as num_submitted,
	p.num_of_questions as question_count,
    p.score_total as score_total,
    p.score_full_total as score_full_total,
	(p.score_total * 100 / p.score_full_total) as score_percentage
FROM oqb.paper p
WHERE
	p.published = 1
	and p.force_answer = 1
    and p.is_teacher = 1
	and p.num_submitted >= 30
	and p.num_of_questions >= 30
	and (p.score_total * 100 / p.score_full_total) >= 50
ORDER BY
	score_percentage DESC;

SELECT 
	trial_counts.paper_id,
	trial_counts.trial_count,
	question_counts.question_count
FROM 
(
	SELECT
		t.paper_id,
		count(*) as trial_count
	FROM
		oqb.trial t
	JOIN oqb.paper p ON
		t.paper_id = p.id
	WHERE
		p.published = 1
		and p.force_answer = 1
	GROUP by
		t.paper_id
) as trial_counts
JOIN
(
	SELECT
		pq.paper_id,
		count(*) as question_count
	FROM
		oqb.paper_question pq
	GROUP by
		pq.paper_id
) as question_counts
ON
	trial_counts.paper_id = question_counts.paper_id

====== SQL : Count number of trial for each paper_id ======
select paper_id, count(*) as trial_count
from oqb.trial t 
GROUP by paper_id

====== SQL : Count number of trial for each paper_id with force_answer = 1 ======
SELECT
	t.paper_id,
	count(*) as trial_count
FROM
	oqb.trial t
JOIN oqb.paper p ON
	t.paper_id = p.id
WHERE
	p.published = 1
    and p.force_answer = 1
GROUP by
	t.paper_id

====== SQL : Count number of question for each paper_id ======
SELECT
	pq.paper_id,
	count(*) as question_count
FROM
	oqb.paper_question pq
GROUP by
	pq.paper_id
